{-# LANGUAGE AllowAmbiguousTypes #-} -- for `idIsEven`

module IR (
    Type (..), ID (..), Name (..), Value (..), Expression (..), Statement (..), Block (..), Transfer (..), Target (..), returnToCaller, typeOf, translate,
    Info (..), LiteralType (..), IdentInfo (..), IdentName (..), render, Style (..), Color (..), defaultStyle
) where


import MyPrelude hiding (BinaryOperator (..))

import qualified Data.Text.Prettyprint.Doc as P

import qualified MyPrelude as AST (BinaryOperator (..))
import qualified AST       as AST
import qualified Name      as AST
import qualified Type      as AST


---------------------------------------------------------------------------------------------------- TYPE DEFINITIONS

data Type node where
    Int        :: Type Expression
    Bool       :: Type Expression
    Parameters :: ![Type Expression] -> Type Block

deriving instance Eq   (Type node)
deriving instance Show (Type node)

data ID node where
    ID      :: !Int      -> ID node
    ASTName :: !AST.Name -> ID Expression
    Return  ::              ID Block

deriving instance Eq   (ID node)
deriving instance Ord  (ID node)
deriving instance Show (ID node)

data Name node = Name {
    ident    :: (ID   node), -- FIXME this needs to be lazy or we get a <<loop>>
    nameType :: !(Type node)
} deriving (Generic, Show)

instance Eq (Name node) where
    (==) = (==) `on` ident

instance Ord (Name node) where
    compare = compare `on` ident

returnToCaller :: Name Block
returnToCaller = Name Return (Parameters [Int])

data Value
    = Literal !Int64
    | Named   !(Name Expression)
    deriving (Generic, Eq, Show)

data Expression
    = Value              !Value
    | UnaryOperator      !UnaryOperator             !Value
    | ArithmeticOperator !Value !ArithmeticOperator !Value
    | ComparisonOperator !Value !ComparisonOperator !Value
    | Ask                !Text
    deriving (Generic, Eq, Show)

data Statement
    = BlockDecl !(Name Block)      !Block
    | Let       !(Name Expression) !Expression
    | Assign    !(Name Expression) !Value
    | Say       !Text
    | Write     !Value
    deriving (Generic, Eq, Show)

data Block = Block {
    arguments :: ![Name Expression],
    body      :: ![Statement],
    transfer  :: !Transfer
} deriving (Generic, Eq, Show)

data Transfer
    = Jump   !Target
    | Branch !Value ![Target] -- targets are in "ascending order": false, then true
    deriving (Generic, Eq, Show)

data Target = Target {
    targetBlock :: !(Name Block),
    targetArgs  :: ![Value]
} deriving (Generic, Eq, Show)

class TypeOf a where
    typeOf :: a -> Type a

instance TypeOf Expression where
    typeOf = \case
        Value (Literal _)        -> Int
        Value (Named name)       -> nameType name
        UnaryOperator Not _      -> Bool
        UnaryOperator Negate _   -> Int
        ArithmeticOperator _ _ _ -> Int
        ComparisonOperator _ _ _ -> Bool
        Ask _                    -> Int

instance TypeOf Block where
    typeOf = Parameters . map nameType . arguments




---------------------------------------------------------------------------------------------------- TRANSLATION FRONTEND

class Monad m => TranslateM m where
    translateName       :: AST.TypedName       -> m (Name Expression)
    emitStatement       :: Statement           -> m ()
    emitLet             :: Maybe AST.TypedName -> Expression -> m (Name Expression)
    emitBlock           :: Type Block          -> m Transfer -> m (Name Block)
    emitTransfer        :: Transfer            -> m ()
    currentBlock        :: m (Name Block)
    currentArguments    :: m [Name Expression]
    currentContinuation :: Type Block -> m (Name Block)

translateTemporary :: TranslateM m => AST.Expression AST.TypedName -> m Value
translateTemporary = translateExpression Nothing

translateBinding :: TranslateM m => AST.TypedName -> AST.Expression AST.TypedName -> m Value
translateBinding = translateExpression . Just

translateExpression :: TranslateM m => Maybe AST.TypedName -> AST.Expression AST.TypedName -> m Value
translateExpression providedName = let emitNamedLet = emitLet providedName in \case
    AST.Named name -> do
        translatedName <- translateName name
        return (Named translatedName)
    AST.Literal num -> do
        let value = Literal (fromIntegral num)
        if isJust providedName
            then do
                name <- emitNamedLet (Value value)
                return (Named name)
            else do
                return value
    AST.UnaryOperator op expr -> do
        value <- translateTemporary expr
        name  <- emitNamedLet (UnaryOperator op value)
        return (Named name)
    AST.BinaryOperator expr1 (AST.ArithmeticOperator op) expr2 -> do
        value1 <- translateTemporary expr1
        value2 <- translateTemporary expr2
        name   <- emitNamedLet (ArithmeticOperator value1 op value2)
        return (Named name)
    AST.BinaryOperator expr1 (AST.ComparisonOperator op) expr2 -> do -- TODO deduplicate these two maybe
        value1 <- translateTemporary expr1
        value2 <- translateTemporary expr2
        name   <- emitNamedLet  (ComparisonOperator value1 op value2)
        return (Named name)
    AST.BinaryOperator expr1 (AST.LogicalOperator op) expr2 -> do
        value1 <- translateTemporary expr1
        joinPoint <- currentContinuation (Parameters [Bool]) -- TODO use the provided name!
        rhsBlock <- emitBlock (Parameters []) $ do
            value2 <- translateTemporary expr2
            return (Jump (Target joinPoint [value2]))
        let branches = case op of
                And -> [Target joinPoint [value1], Target rhsBlock  []]
                Or  -> [Target rhsBlock  [],       Target joinPoint [value1]]
        emitTransfer (Branch value1 branches)
        args <- currentArguments
        return (Named (assert (head args)))
    AST.Ask text -> do
        name <- emitNamedLet (Ask text)
        return (Named name)

translateStatement :: TranslateM m => AST.Statement AST.TypedName -> m ()
translateStatement = \case
    AST.Binding _ name expr -> do
        _ <- translateBinding name expr
        return ()
    AST.Assign name expr -> do
        translatedName <- translateName name
        value <- translateTemporary expr
        emitStatement (Assign translatedName value)
    AST.IfThen expr block -> do
        value <- translateTemporary expr
        joinPoint <- currentContinuation (Parameters [])
        thenBlock <- emitBlock (Parameters []) $ do
            translateBlock block
            return (Jump (Target joinPoint []))
        emitTransfer (Branch value [Target joinPoint [], Target thenBlock []])
    AST.IfThenElse expr block1 block2 -> do
        value <- translateTemporary expr
        joinPoint <- currentContinuation (Parameters [])
        thenBlock <- emitBlock (Parameters []) $ do
            translateBlock block1
            return (Jump (Target joinPoint []))
        elseBlock <- emitBlock (Parameters []) $ do
            translateBlock block2
            return (Jump (Target joinPoint []))
        emitTransfer (Branch value [Target elseBlock [], Target thenBlock []])
    AST.Forever block -> do
        foreverBlock <- emitBlock (Parameters []) $ do
            blockBody <- currentBlock
            translateBlock block
            return (Jump (Target blockBody []))
        emitTransfer (Jump (Target foreverBlock []))
    AST.While expr block -> do
        joinPoint <- currentContinuation (Parameters [])
        whileBlock <- emitBlock (Parameters []) $ do
            conditionTest <- currentBlock
            blockBody <- emitBlock (Parameters []) $ do
                translateBlock block
                return (Jump (Target conditionTest []))
            value <- translateTemporary expr
            return (Branch value [Target joinPoint [], Target blockBody []])
        emitTransfer (Jump (Target whileBlock []))
    AST.Return maybeExpr -> do
        maybeValue <- mapM translateTemporary maybeExpr
        emitTransfer (Jump (Target returnToCaller [fromMaybe (Literal 0) maybeValue]))
    AST.Break -> do
        todo -- TODO
    AST.Say text -> do
        emitStatement (Say text)
    AST.Write expr -> do
        value <- translateTemporary expr
        emitStatement (Write value)

translateBlock :: TranslateM m => AST.Block AST.TypedName -> m ()
translateBlock (AST.Block statements) = mapM_ translateStatement statements




---------------------------------------------------------------------------------------------------- TRANSLATION BACKEND

translate :: AST.Block AST.TypedName -> Block
translate = evalTardis (backwardsState, forwardsState) . runTranslate . translateRootBlock
    where
        backwardsState = BackwardsState Nothing Nothing Nothing
        forwardsState  = ForwardsState  { lastID = 0, innermostBlock = BlockState (ID 0) [] [] Nothing Nothing }
        translateRootBlock rootBlock = do
            (blockID, finishedBlock) <- liftM assert (backGetM (field @"thisBlock"))
            translateBlock rootBlock
            emitTransfer (Jump (Target returnToCaller [Literal 0]))
            --assertM (blockID == ID 0) -- this results in a <<loop>>
            return finishedBlock

newtype Translate a = Translate {
    runTranslate :: Tardis BackwardsState ForwardsState a
} deriving (Functor, Applicative, Monad, MonadFix, MonadState ForwardsState, MonadTardis BackwardsState ForwardsState)

data ForwardsState = ForwardsState {
    lastID         :: !Int,
    innermostBlock :: !BlockState
} deriving Generic

data BlockState = BlockState {
    blockID             :: !(ID Block),
    blockArguments      :: ![Name Expression],
    statements          :: ![Statement],
    emittedContinuation :: !(Maybe (Name Block)),
    enclosingBlock      :: !(Maybe BlockState)
} deriving Generic

data BackwardsState = BackwardsState {
    nextBlock              :: !(Maybe (ID Block, Block)),
    thisBlock              :: !(Maybe (ID Block, Block)),
    enclosingContinuations :: !(Maybe BackwardsState)
} deriving Generic

class NewID node where
    idIsEven :: Bool

instance NewID Block where
    idIsEven = False

instance NewID Expression where
    idIsEven = True

newID :: forall node. NewID node => Translate (ID node)
newID = do
    -- NOTE incrementing first is significant, ID 0 is the root block!
    modifyM (field @"lastID") $ \lastID ->
        lastID + (if idIsEven @node == (lastID % 2 == 0) then 2 else 1)
    new <- getM (field @"lastID")
    return (ID new)

newArgumentIDs :: Type Block -> Translate [Name Expression]
newArgumentIDs (Parameters argTypes) = do
    forM argTypes $ \argType -> do
        argID <- newID
        return (Name argID argType)

pushBlock :: Type Block -> Translate ()
pushBlock params = do
    blockID <- newID
    args    <- newArgumentIDs params
    modifyM         (field @"innermostBlock") (\previouslyInnermost -> BlockState blockID args [] Nothing (Just previouslyInnermost))
    backModifyState (assert . enclosingContinuations)

popBlock :: Translate ()
popBlock = do
    modifyM         (field @"innermostBlock") (assert . enclosingBlock)
    backModifyState (\previousState -> BackwardsState Nothing Nothing (Just previousState))


instance TranslateM Translate where
    translateName :: AST.TypedName -> Translate (Name Expression)
    translateName (AST.NameWith name ty) = do
        return (Name (ASTName name) (translatedType ty))
        where translatedType = \case
                AST.Bool -> Bool
                AST.Int  -> Int

    emitStatement :: Statement -> Translate ()
    emitStatement statement = do
        modifyM (field @"innermostBlock" . field @"statements") (++ [statement])

    emitLet :: Maybe AST.TypedName -> Expression -> Translate (Name Expression)
    emitLet providedName expr = do
        name <- case providedName of
            Just astName -> do
                translatedName <- translateName astName
                assertM (nameType translatedName == typeOf expr)
                return translatedName
            Nothing -> do
                letID <- newID
                return (Name letID (typeOf expr))
        emitStatement (Let name expr)
        return name

    emitBlock :: Type Block -> Translate Transfer -> Translate (Name Block)
    emitBlock paramTypes translateBody = do
        pushBlock paramTypes
        blockName                <- currentBlock
        ~(blockID, finishedBlock) <- liftM assert (backGetM (field @"thisBlock")) -- FIXME need a lazy match here to avoid a <<loop>>
        transferAtEnd            <- translateBody
        --assertM (blockID == ident blockName) -- this results in a <<loop>>
        emitTransfer transferAtEnd
        popBlock
        emitStatement (BlockDecl blockName finishedBlock)
        return blockName

    emitTransfer :: Transfer -> Translate ()
    emitTransfer transfer = do
        BlockState { blockID, blockArguments, statements, emittedContinuation, enclosingBlock } <- getM (field @"innermostBlock")
        let nextBlockParams = case emittedContinuation of
                Just blockName -> nameType blockName
                Nothing        -> Parameters [] -- TODO this means we're in dead code; should we do anything about it??
        nextBlockID   <- newID -- FIXME we should skip allocating a block if we're at the end of a parent block!
        nextBlockArgs <- newArgumentIDs nextBlockParams
        setM (field @"innermostBlock") (BlockState nextBlockID nextBlockArgs [] Nothing enclosingBlock)
        backModifyState $ \BackwardsState { nextBlock = _, thisBlock = prevThisBlock, enclosingContinuations } ->
            BackwardsState { nextBlock = prevThisBlock, thisBlock = Just (blockID, Block blockArguments statements transfer), enclosingContinuations }

    currentBlock :: Translate (Name Block)
    currentBlock = do
        blockID   <- getM (field @"innermostBlock" . field @"blockID")
        arguments <- currentArguments
        return (Name blockID (Parameters (map nameType arguments)))

    currentArguments :: Translate [Name Expression]
    currentArguments = do
        getM (field @"innermostBlock" . field @"blockArguments")

    currentContinuation :: Type Block -> Translate (Name Block)
    currentContinuation params = do
        alreadyEmitted <- getM (field @"innermostBlock" . field @"emittedContinuation")
        case alreadyEmitted of
            Nothing -> do
                ~(nextBlockID, nextBlock) <- liftM assert (backGetM (field @"nextBlock")) -- FIXME need a lazy match here to avoid a <<loop>>
                let nextBlockName = Name nextBlockID params
                emitStatement (BlockDecl nextBlockName nextBlock)
                setM (field @"innermostBlock" . field @"emittedContinuation") (Just nextBlockName)
                return nextBlockName
            Just nextBlockName -> do
                assertM (params == nameType nextBlockName)
                return nextBlockName

{- EXAMPLE INPUT
main1
if (foo) {
    body1
}
main2
if (foo2) {
    body2
}
main3
-}

{- EXAMPLE OUTPUT
block main() {
    main1
    block join1() {
        main2
        block join2() {
            main3
            return
        }
        block if2() {
            body2
            jump join2()
        }
        branch foo2 [if2, join2]
    }
    block if1() {
        body1
        jump join1()
    }
    branch foo [if1, join1]
}
-}



---------------------------------------------------------------------------------------------------- PRETTY PRINTING

-- TODO bikeshed the names of all these things

data Info
    = Keyword
    | Brace
    | Paren
    | DefineEquals
    | AssignEquals
    | Colon
    | UserOperator
    | Literal'   !LiteralType
    | Sigil      !IdentInfo
    | Identifier !IdentInfo
    deriving (Generic, Eq, Show)

data LiteralType
    = NumberLiteral
    | BoolLiteral
    | StringLiteral
    deriving (Generic, Eq, Show)

data IdentInfo = IdentInfo {
    isDefinition :: !Bool,
    identName    :: !IdentName
} deriving (Generic, Eq, Show)

-- TODO maybe we should replace everything with plain `Text`s, so we can share definitions w/ an AST pretty printer?
data IdentName
    = LetName    !(Name Expression)
    | BlockName  !(Name Block)
    | TypeName   !(Type Expression)
    | GlobalName !Text
    deriving (Generic, Eq, Show)

render :: Block -> Doc Info
render rootBlock = renderBody (body rootBlock) (transfer rootBlock) where
    note         = P.annotate
    keyword      = note Keyword
    operator     = note UserOperator
    colon        = note Colon ":"
    defineEquals = note DefineEquals "="
    assignEquals = note AssignEquals "="
    string       = note (Literal' StringLiteral) . P.dquotes . P.pretty
    number       = note (Literal' NumberLiteral) . P.pretty
    builtin      = renderName . IdentInfo False . GlobalName
    type'        = renderName . IdentInfo False . TypeName
    blockID def  = renderName . IdentInfo def   . BlockName
    letID   def  = renderName . IdentInfo def   . LetName
    braces  doc  = note Brace "{" ++ doc ++ note Brace "}"
    parens  doc  = note Paren "(" ++ doc ++ note Paren ")"

    renderBody statements transfer = mconcat (map (P.hardline ++) (map renderStatement statements ++ [renderTransfer transfer]))

    renderStatement = \case
        BlockDecl name block -> keyword "block"  ++ " " ++ blockID True name ++ argumentList (arguments block) ++ " " ++ braces (P.nest 4 (renderBody (body block) (transfer block)) ++ P.hardline)
        Let       name expr  -> keyword "let"    ++ " " ++ typedName name ++ " " ++ defineEquals ++ " " ++ renderExpr expr
        Assign    name value -> letID False name ++ " " ++ assignEquals ++ " " ++ renderValue value
        Say       text       -> builtin "say"    ++ parens (string text)
        Write     value      -> builtin "write"  ++ parens (renderValue value)

    renderTransfer = \case
        Jump         target  -> keyword "jump"   ++ " " ++ renderTarget target
        Branch value targets -> keyword "branch" ++ " " ++ renderValue value ++ " " ++ P.hsep (map renderTarget targets)

    renderTarget target = blockID False (targetBlock target) ++ parens (P.hsep (P.punctuate "," (map renderValue (targetArgs target))))

    renderExpr = \case
        Value              value            -> renderValue value
        UnaryOperator      op value         -> unaryOp op ++ renderValue value
        ArithmeticOperator value1 op value2 -> renderValue value1 ++ " " ++ arithOp op ++ " " ++ renderValue value2
        ComparisonOperator value1 op value2 -> renderValue value1 ++ " " ++ cmpOp   op ++ " " ++ renderValue value2
        Ask                text             -> builtin "ask" ++ parens (string text)

    renderValue = \case
        Named   name -> letID False name
        Literal num  -> number num

    renderName :: IdentInfo -> Doc Info
    renderName info = note (Sigil info) sigil ++ note (Identifier info) name where
        (sigil, name) = case identName info of
            LetName    n -> ("$", renderIdent (ident n))
            BlockName  n -> ("%", renderIdent (ident n))
            TypeName   t -> ("",  P.pretty (show t))
            GlobalName n -> ("",  P.pretty n)

    renderIdent :: ID node -> Doc Info
    renderIdent = \case
        ASTName n -> P.pretty (AST.givenName n)
        ID      i -> P.pretty i
        Return    -> keyword "return" -- FIXME this gets tagged as both a Keyword and an Identifier, but it seems to work out OK

    argumentList args = parens (P.hsep (P.punctuate "," (map typedName args)))

    typedName name = letID True name ++ colon ++ " " ++ type' (nameType name)

    -- FIXME: Deduplicate these with `module Token` maybe?? Put them in MyPrelude?
    unaryOp = operator . \case
        Not    -> "!"
        Negate -> "-"
    arithOp = operator . \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
    cmpOp = operator . \case
        Equal        -> "=="
        NotEqual     -> "!="
        Less         -> "<"
        LessEqual    -> "<="
        Greater      -> ">"
        GreaterEqual -> ">="

data Style = Style {
    color        :: !(Maybe Color),
    isDull       :: !Bool,
    isBold       :: !Bool,
    isItalic     :: !Bool,
    isUnderlined :: !Bool
} deriving (Generic, Eq, Show)

data Color
    = Black
    | White
    | Red
    | Green
    | Blue
    | Cyan
    | Magenta
    | Yellow
    deriving (Generic, Eq, Show)

defaultStyle :: Info -> Style
defaultStyle = \case
    Keyword          -> plain { isBold = True }
    Brace            -> plain { isBold = True }
    Paren            -> plain
    DefineEquals     -> plain { isBold = True }
    AssignEquals     -> plain { color  = Just Magenta }
    Colon            -> plain { isBold = True }
    UserOperator     -> plain { color  = Just Magenta }
    Literal'   _     -> plain { color = Just Yellow }
    Sigil      info  -> plain { isUnderlined = isDefinition info }
                                --color = nameColor (identName info) }
    Identifier info  -> plain { isUnderlined = isDefinition info,
                                color        = nameColor (identName info) } -- TODO, based on type or ID!
    where
        plain = Style Nothing False False False False
        nameColor = \case
            LetName    _ -> Just Green
            BlockName  _ -> Just Red
            TypeName   _ -> Just Cyan
            GlobalName _ -> Nothing