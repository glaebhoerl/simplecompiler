module IR (Type (..), ID (..), Name (..), Value (..), Expression (..), Statement (..), Block (..), Transfer (..), Target (..), returnToCaller, typeOf, translate, render) where

import MyPrelude hiding (BinaryOperator (..))

import qualified Data.Text.Prettyprint.Doc as P

import qualified MyPrelude as AST (BinaryOperator (..))
import qualified AST       as AST
import qualified Name      as AST
import qualified Type      as AST

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

render :: Block -> Doc a
render = renderStatement . BlockDecl (Name (ID 0) (Parameters [])) where
    renderStatement = \case
        BlockDecl name block -> "block " ++ renderBlockID (ident name) ++ renderArguments (arguments block) ++ " " ++ renderBody (body block) (transfer block)
        Let       name expr  -> "let "   ++ renderTypedName name     ++ " = " ++ renderExpr expr
        Assign    name value -> renderLetID (ident name) ++ " = " ++ renderValue value
        Say       text       -> "say"   ++ P.parens (P.dquotes (P.pretty text))
        Write     value      -> "write" ++ P.parens (renderValue value)

    renderBody statements transfer = P.braces (P.nest 4 (mconcat (map (P.hardline ++) (map renderStatement statements ++ [renderTransfer transfer]))) ++ P.hardline)

    renderTransfer = \case
        Jump         target  -> "jump "   ++ renderTarget target
        Branch value targets -> "branch " ++ renderValue value ++ " " ++ P.brackets (P.hsep (P.punctuate "," (map renderTarget targets)))

    renderTarget target = renderBlockID (ident (targetBlock target)) ++ P.parens (P.hsep (P.punctuate "," (map renderValue (targetArgs target))))

    renderExpr = \case
        Value              value            -> renderValue value
        UnaryOperator      op value         -> renderUnaryOp op ++ renderValue value
        ArithmeticOperator value1 op value2 -> renderValue value1 ++ " " ++ renderArithOp op ++ " " ++ renderValue value2
        ComparisonOperator value1 op value2 -> renderValue value1 ++ " " ++ renderCmpOp   op ++ " " ++ renderValue value2
        Ask                text             -> "ask" ++ P.parens (P.dquotes (P.pretty text))

    renderValue = \case
        Named   name   -> renderLetID (ident name)
        Literal number -> P.pretty number

    renderLetID :: ID Expression -> Doc a
    renderLetID = \case
        ASTName astName -> "%" ++ P.pretty (AST.givenName astName)
        ID      number  -> "%" ++ P.pretty number

    renderBlockID :: ID Block -> Doc a
    renderBlockID = \case
        ID     number -> "@" ++ P.pretty number
        Return        -> "@return"

    renderArguments args = P.parens (P.hsep (P.punctuate "," (map renderTypedName args)))

    renderTypedName name = renderLetID (ident name) ++ ": " ++ renderType (nameType name)

    renderType :: Type Expression -> Doc a
    renderType = P.pretty . show

    -- FIXME: Deduplicate these with `module Token` maybe??
    renderUnaryOp = \case
        Not    -> "!"
        Negate -> "-"
    renderArithOp = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"
    renderCmpOp = \case
        Equal        -> "=="
        NotEqual     -> "!="
        Less         -> "<"
        LessEqual    -> "<="
        Greater      -> ">"
        GreaterEqual -> ">="

class Monad m => TranslateM m where
    translateName       :: AST.TypedName -> m (Name Expression)
    emitStatement       :: Statement     -> m ()
    emitLet             :: Expression    -> m (Name Expression)
    emitBlock           :: Type Block    -> m Transfer -> m (Name Block)
    emitTransfer        :: Transfer      -> m ()
    currentBlock        :: m (Name Block)
    currentArguments    :: m [Name Expression]
    currentContinuation :: Type Block -> m (Name Block)

translateExpression :: TranslateM m => AST.Expression AST.TypedName -> m Value
translateExpression = \case
    AST.Named name -> do
        translatedName <- translateName name
        return (Named translatedName)
    AST.Literal num -> do
        return (Literal (fromIntegral num))
    AST.UnaryOperator op expr -> do
        value <- translateExpression expr
        name  <- emitLet (UnaryOperator op value)
        return (Named name)
    AST.BinaryOperator expr1 (AST.ArithmeticOperator op) expr2 -> do
        value1 <- translateExpression expr1
        value2 <- translateExpression expr2
        name   <- emitLet (ArithmeticOperator value1 op value2)
        return (Named name)
    AST.BinaryOperator expr1 (AST.ComparisonOperator op) expr2 -> do -- TODO deduplicate these two maybe
        value1 <- translateExpression expr1
        value2 <- translateExpression expr2
        name   <- emitLet (ComparisonOperator value1 op value2)
        return (Named name)
    AST.BinaryOperator expr1 (AST.LogicalOperator op) expr2 -> do
        value1 <- translateExpression expr1
        joinPoint <- currentContinuation (Parameters [Bool])
        rhsBlock <- emitBlock (Parameters []) $ do
            value2 <- translateExpression expr2
            return (Jump (Target joinPoint [value2]))
        let branches = case op of
                And -> [Target joinPoint [value1], Target rhsBlock  []]
                Or  -> [Target rhsBlock  [],       Target joinPoint [value1]]
        emitTransfer (Branch value1 branches)
        args <- currentArguments
        return (Named (assert (head args)))
    AST.Ask text -> do
        name <- emitLet (Ask text)
        return (Named name)

translateStatement :: TranslateM m => AST.Statement AST.TypedName -> m ()
translateStatement = \case
    AST.Binding _ name expr -> do
        translatedName <- translateName name
        value <- translateExpression expr
        emitStatement (Let translatedName (Value value)) -- (this is slightly redundant, but it should get optimized away)
    AST.Assign name expr -> do
        translatedName <- translateName name
        value <- translateExpression expr
        emitStatement (Assign translatedName value)
    AST.IfThen expr block -> do
        value <- translateExpression expr
        joinPoint <- currentContinuation (Parameters [])
        thenBlock <- emitBlock (Parameters []) $ do
            translateBlock block
            return (Jump (Target joinPoint []))
        emitTransfer (Branch value [Target joinPoint [], Target thenBlock []])
    AST.IfThenElse expr block1 block2 -> do
        value <- translateExpression expr
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
            value <- translateExpression expr
            return (Branch value [Target joinPoint [], Target blockBody []])
        emitTransfer (Jump (Target whileBlock []))
    AST.Return maybeExpr -> do
        maybeValue <- mapM translateExpression maybeExpr
        emitTransfer (Jump (Target returnToCaller [fromMaybe (Literal 0) maybeValue]))
    AST.Break -> do
        todo -- TODO
    AST.Say text -> do
        emitStatement (Say text)
    AST.Write expr -> do
        value <- translateExpression expr
        emitStatement (Write value)

translateBlock :: TranslateM m => AST.Block AST.TypedName -> m ()
translateBlock (AST.Block statements) = mapM_ translateStatement statements


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

newID :: Translate (ID node)
newID = do
    -- NOTE incrementing first is significant, ID 0 is the root block!
    (field @"lastID") += 1
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

    emitLet :: Expression -> Translate (Name Expression)
    emitLet expr = do
        letID <- newID
        let name = Name letID (typeOf expr)
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
