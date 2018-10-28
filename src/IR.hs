{-# LANGUAGE AllowAmbiguousTypes #-} -- for `idIsEven`

module IR (
    Type (..), ID (..), Name (..), Literal (..), Value (..), Expression (..), Statement (..), Block (..), Transfer (..), Target (..), paramTypes, returnToCaller, typeOf, translate,
    ValidationError (..), validate, eliminateTrivialBlocks
) where

import MyPrelude

import qualified Data.Map as Map

import qualified Pretty as P
import qualified AST    as AST
import qualified Name   as AST
import qualified Type   as AST

import Pretty (Render, render)


---------------------------------------------------------------------------------------------------- TYPE DEFINITIONS

data Type node where
    Int        :: Type Expression
    Bool       :: Type Expression
    Text       :: Type Expression
    Parameters :: ![Type Expression] -> Type Block

paramTypes :: Type Block -> [Type Expression]
paramTypes (Parameters types) = types

deriving instance Eq   (Type node)
deriving instance Show (Type node)

data ID node where
    LetID   :: !Int      -> ID Expression
    ASTName :: !AST.Name -> ID Expression
    BlockID :: !Int      -> ID Block
    Return  ::              ID Block

deriving instance Eq   (ID node)
deriving instance Ord  (ID node)
deriving instance Show (ID node)

data Name node = Name {
    ident       :: !(ID   node),
    nameType    :: !(Type node),
    description :: !Text
} deriving (Generic, Show)

instance Eq (Name node) where
    (==) = (==) `on` ident

instance Ord (Name node) where
    compare = compare `on` ident

returnToCaller :: Name Block
returnToCaller = Name Return (Parameters [Int]) ""

data Literal
    = Number !Int64
    | String !Text
    deriving (Generic, Eq, Show)

data Value
    = Literal !Literal
    | Named   !(Name Expression)
    deriving (Generic, Eq, Show)

data Expression
    = Value          !Value
    | UnaryOperator  !UnaryOperator !Value
    | BinaryOperator !Value !BinaryOperator !Value
    | Ask            !Value
    deriving (Generic, Eq, Show)

data Statement
    = BlockDecl !(Name Block)      !Block
    | Let       !(Name Expression) !Expression
    | Assign    !(Name Expression) !Value
    | Say       !Value
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
        Value (Literal literal)  -> case literal of
            Number _             -> Int
            String _             -> Text
        Value (Named name)       -> nameType name
        UnaryOperator Not      _ -> Bool
        UnaryOperator Negate   _ -> Int
        BinaryOperator _ op    _ -> case op of
            ArithmeticOperator _ -> Int
            ComparisonOperator _ -> Bool
            LogicalOperator    _ -> Bool
        Ask                    _ -> Int

instance TypeOf Block where
    typeOf = Parameters . map nameType . arguments




---------------------------------------------------------------------------------------------------- TRANSLATION FRONTEND

class Monad m => TranslateM m where
    translateName       :: AST.TypedName       -> m (Name Expression)
    emitStatement       :: Statement           -> m ()
    emitLet             :: Maybe AST.TypedName -> Expression -> m (Name Expression)
    emitBlock           :: Text -> Type Block  -> m Transfer -> m (Name Block)
    emitContinuation    :: Text -> Type Block                -> m (Name Block) -- TODO add `Maybe AST.TypedName` or something
    emitTransfer        :: Transfer            -> m ()
    currentBlock        :: m (Name Block)
    currentArguments    :: m [Name Expression]

translateTemporary :: TranslateM m => AST.Expression AST.TypedName -> m Value
translateTemporary = translateExpression Nothing

translateBinding :: TranslateM m => AST.TypedName -> AST.Expression AST.TypedName -> m Value
translateBinding = translateExpression . Just

translateExpression :: TranslateM m => Maybe AST.TypedName -> AST.Expression AST.TypedName -> m Value
translateExpression providedName = let emitNamedLet = emitLet providedName in \case
    AST.Named name -> do
        translatedName <- translateName name
        return (Named translatedName)
    AST.NumberLiteral num -> do
        let value = Literal (Number (fromIntegral num))
        if isJust providedName
            then do
                name <- emitNamedLet (Value value)
                return (Named name)
            else do
                return value
    AST.TextLiteral text -> do -- TODO refactor
        let value = Literal (String text)
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
    -- Logical operators are short-circuiting, so we can't just emit them as simple statements, except when the RHS is already a Value.
    AST.BinaryOperator expr1 (LogicalOperator op) expr2 | (expr2 `isn't` constructor @"NumberLiteral" && expr2 `isn't` constructor @"Named") -> do
        value1 <- translateTemporary expr1
        let opName = toLower (showText op)
        joinPoint <- emitContinuation ("join_" ++ opName) (Parameters [Bool]) -- TODO use the provided name for the arg!
        rhsBlock <- emitBlock opName (Parameters []) $ do
            value2 <- translateTemporary expr2
            return (Jump (Target joinPoint [value2]))
        let branches = case op of
                And -> [Target joinPoint [value1], Target rhsBlock  []]
                Or  -> [Target rhsBlock  [],       Target joinPoint [value1]]
        emitTransfer (Branch value1 branches)
        args <- currentArguments
        return (Named (assert (head args)))
    AST.BinaryOperator expr1 op expr2 -> do
        value1 <- translateTemporary expr1
        value2 <- translateTemporary expr2
        name   <- emitNamedLet (BinaryOperator value1 op value2)
        return (Named name)
    AST.Ask expr -> do
        value <- translateTemporary expr
        name  <- emitNamedLet (Ask value)
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
        joinPoint <- emitContinuation "join_if" (Parameters [])
        thenBlock <- emitBlock "if" (Parameters []) $ do
            translateBlock block
            return (Jump (Target joinPoint []))
        emitTransfer (Branch value [Target joinPoint [], Target thenBlock []])
    AST.IfThenElse expr block1 block2 -> do
        value <- translateTemporary expr
        joinPoint <- emitContinuation "join_if_else" (Parameters [])
        thenBlock <- emitBlock "if" (Parameters []) $ do
            translateBlock block1
            return (Jump (Target joinPoint []))
        elseBlock <- emitBlock "else" (Parameters []) $ do
            translateBlock block2
            return (Jump (Target joinPoint []))
        emitTransfer (Branch value [Target elseBlock [], Target thenBlock []])
    AST.Forever block -> do
        foreverBlock <- emitBlock "forever" (Parameters []) $ do
            blockBody <- currentBlock
            translateBlock block
            return (Jump (Target blockBody []))
        emitTransfer (Jump (Target foreverBlock []))
    AST.While expr block -> do
        joinPoint <- emitContinuation "join_while" (Parameters [])
        whileBlock <- emitBlock "while" (Parameters []) $ do
            conditionTest <- currentBlock
            blockBody <- emitBlock "while_body" (Parameters []) $ do
                translateBlock block
                return (Jump (Target conditionTest []))
            value <- translateTemporary expr
            return (Branch value [Target joinPoint [], Target blockBody []])
        emitTransfer (Jump (Target whileBlock []))
    AST.Return maybeExpr -> do
        maybeValue <- mapM translateTemporary maybeExpr
        emitTransfer (Jump (Target returnToCaller [fromMaybe (Literal (Number 0)) maybeValue]))
    AST.Break -> do
        todo -- TODO
    AST.Say expr -> do
        value <- translateTemporary expr
        emitStatement (Say value)
    AST.Write expr -> do
        value <- translateTemporary expr
        emitStatement (Write value)

translateBlock :: TranslateM m => AST.Block AST.TypedName -> m ()
translateBlock (AST.Block statements) = mapM_ translateStatement statements




---------------------------------------------------------------------------------------------------- TRANSLATION BACKEND

translate :: AST.Block AST.TypedName -> Block
translate = evalState initialState . runTranslate . translateRootBlock
    where
        initialState  = TranslateState  { lastID = 0, innermostBlock = BlockState (BlockID 0) "root" [] [] NoContinuation (Just (innermostBlock initialState)) } -- HACK
        translateRootBlock rootBlock = do
            translateBlock rootBlock
            emitTransfer (Jump (Target returnToCaller [Literal (Number 0)]))
            liftM (snd . assert . getWhen (constructor @"BlockDecl") . assert . head) (getM (field @"innermostBlock" . field @"statements")) -- lol :(

newtype Translate a = Translate {
    runTranslate :: State TranslateState a
} deriving (Functor, Applicative, Monad, MonadState TranslateState)

data TranslateState = TranslateState {
    lastID         :: !Int,
    innermostBlock :: !BlockState
} deriving (Generic, Eq, Show)

data BlockState = BlockState {
    blockID           :: !(ID Block),
    blockDescription  :: !Text,
    blockArguments    :: ![Name Expression],
    statements        :: ![Statement],
    continuationState :: !ContinuationState,
    enclosingBlock    :: !(Maybe BlockState)
} deriving (Generic, Eq, Show)

data ContinuationState
    = NoContinuation
    | EmittedContinuation !BlockState -- the `enclosingBlock` field is left `Nothing`, filled in later!
    | EmittedTransfer     !Transfer
    deriving (Generic, Eq, Show)

class NewID node where
    idIsEven :: Bool
    makeID :: Int -> ID node

instance NewID Expression where
    idIsEven = True
    makeID = LetID

instance NewID Block where
    idIsEven = False
    makeID = BlockID

newID :: forall node. NewID node => Translate (ID node)
newID = do
    -- NOTE incrementing first is significant, ID 0 is the root block!
    modifyM (field @"lastID") $ \lastID ->
        lastID + (if idIsEven @node == (lastID % 2 == 0) then 2 else 1)
    new <- getM (field @"lastID")
    return (makeID new)

newArgumentIDs :: Type Block -> Translate [Name Expression]
newArgumentIDs (Parameters argTypes) = do
    forM argTypes $ \argType -> do
        argID <- newID
        return (Name argID argType "")

instance TranslateM Translate where
    translateName :: AST.TypedName -> Translate (Name Expression)
    translateName (AST.NameWith name ty) = do
        return (Name (ASTName name) (translatedType ty) (AST.givenName name))
        where translatedType = \case
                AST.Bool -> Bool
                AST.Int  -> Int
                AST.Text -> Text

    emitStatement :: Statement -> Translate ()
    emitStatement statement = do
        modifyM (field @"innermostBlock" . field @"statements") (++ [statement])
        return ()

    emitLet :: Maybe AST.TypedName -> Expression -> Translate (Name Expression)
    emitLet providedName expr = do
        name <- case providedName of
            Just astName -> do
                translatedName <- translateName astName
                assertM (nameType translatedName == typeOf expr)
                return translatedName
            Nothing -> do
                letID <- newID
                return (Name letID (typeOf expr) "")
        emitStatement (Let name expr)
        return name

    emitBlock :: Text -> Type Block -> Translate Transfer -> Translate (Name Block)
    emitBlock description argTypes translateBody = do
        blockID <- newID
        args    <- newArgumentIDs argTypes
        modifyM (field @"innermostBlock") (\previouslyInnermost -> BlockState blockID description args [] NoContinuation (Just previouslyInnermost))
        blockName     <- currentBlock
        transferAtEnd <- translateBody
        emitTransfer transferAtEnd -- this will handle popping and emitting the finished block
        return blockName

    emitContinuation :: Text -> Type Block -> Translate (Name Block)
    emitContinuation nextBlockDescription nextBlockParams = do
        alreadyEmitted <- getM (field @"innermostBlock" . field @"continuationState")
        assertM (alreadyEmitted == NoContinuation)
        nextBlockID    <- newID
        nextBlockArgs  <- newArgumentIDs nextBlockParams
        let nextBlockName = Name nextBlockID nextBlockParams nextBlockDescription
        let nextBlockStub = Block { arguments = nextBlockArgs, body = [], transfer = Jump (Target returnToCaller []) }
        emitStatement (BlockDecl nextBlockName nextBlockStub)
        setM (field @"innermostBlock" . field @"continuationState") (EmittedContinuation (BlockState nextBlockID nextBlockDescription nextBlockArgs [] NoContinuation Nothing))
        return nextBlockName

    -- does this black have an emitted continuation?
    --   yes -> push a new block with this block as the parent block, with the name/args of the emitted continuation
    --   no  -> is this block the continuation of the enclosing block?
    --      yes -> pop this block, then replace the continuation stub in the parent block with this block
    --               then recurse from "is this block the continuation...?" for the parent block
    --      no  -> pop this block, append this block as a blockdecl statement to the parent block
    emitTransfer :: Transfer -> Translate ()
    emitTransfer transfer = do
        BlockState { blockArguments, statements, continuationState, blockID } <- getM (field @"innermostBlock")
        case continuationState of
            EmittedTransfer _ -> bug "Transfer already emitted!"
            EmittedContinuation continuationBlock -> do
                setM (field @"innermostBlock" . field @"continuationState") (EmittedTransfer transfer)
                modifyM (field @"innermostBlock") (\previouslyInnermost -> continuationBlock { enclosingBlock = Just previouslyInnermost })
                return ()
            NoContinuation -> do
                blockName <- currentBlock
                let finishedBlock = Block blockArguments statements transfer
                modifyM (field @"innermostBlock") (assert . enclosingBlock)
                parentContinuationState <- getM (field @"innermostBlock" . field @"continuationState")
                case parentContinuationState of
                    EmittedTransfer parentTransfer -> do
                        modifyM (field @"innermostBlock" . field @"statements") $ map $ \case
                            BlockDecl name _ | name == blockName -> BlockDecl blockName finishedBlock
                            otherStatement                       -> otherStatement
                        setM (field @"innermostBlock" . field @"continuationState") NoContinuation
                        emitTransfer parentTransfer
                    _ -> do
                        emitStatement (BlockDecl blockName finishedBlock)

    currentBlock :: Translate (Name Block)
    currentBlock = do
        blockID     <- getM (field @"innermostBlock" . field @"blockID")
        description <- getM (field @"innermostBlock" . field @"blockDescription")
        arguments   <- currentArguments
        return (Name blockID (Parameters (map nameType arguments)) description)

    currentArguments :: Translate [Name Expression]
    currentArguments = do
        getM (field @"innermostBlock" . field @"blockArguments")


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




---------------------------------------------------------------------------------------------------- VALIDATION

data ValidationError where
    NotInScope     :: !(ID node)                         -> ValidationError
    Redefined      :: !(ID node)                         -> ValidationError
    Inconsistent   :: !(Name node) -> !(Name node)       -> ValidationError
    TypeMismatch   :: Show node => !(Type node) -> !node -> ValidationError -- Technically we don't need the `Show` here, but `deriving` doesn't know that.
    BadTargetCount :: ![Target]                          -> ValidationError
    BadArgsCount   :: !Target                            -> ValidationError

deriving instance Show ValidationError

data Scope = Scope {
    lets   :: !(Map (ID Expression) (Type Expression)),
    blocks :: !(Map (ID Block)      (Type Block)),
    parent :: !(Maybe Scope)
} deriving Generic

-- this is basically the use case for `dependent-map`, but this seems simpler for now
insertID :: ID node -> Type node -> Scope -> Scope
insertID ident nameType = case ident of
    Return    -> bug "Tried to insert the special builtin `return` block into the context!"
    BlockID _ -> modify (field @"blocks") (Map.insert ident nameType)
    LetID   _ -> modify (field @"lets")   (Map.insert ident nameType)
    ASTName _ -> modify (field @"lets")   (Map.insert ident nameType)

lookupID :: ID node -> Scope -> Maybe (Type node)
lookupID ident Scope { lets, blocks, parent } = case ident of
    Return    -> Just (nameType returnToCaller)
    BlockID _ -> orLookupInParent (Map.lookup ident blocks)
    LetID   _ -> orLookupInParent (Map.lookup ident lets)
    ASTName _ -> orLookupInParent (Map.lookup ident lets)
    where orLookupInParent = maybe (join (fmap (lookupID ident) parent)) Just

memberID :: ID node -> Scope -> Bool
memberID ident = isJust . lookupID ident

validate :: Block -> Either ValidationError ()
validate = runExcept . evalStateT (Scope Map.empty Map.empty Nothing) . checkBlock (Parameters []) where
    checkBlock expectedType block = do
        checkType expectedType block
        modifyState (\parent -> Scope Map.empty Map.empty (Just parent))
        mapM_ recordID       (arguments block)
        mapM_ checkStatement (body      block)
        checkTransfer        (transfer  block)
        modifyState (assert . parent)
        return ()
    checkStatement = \case
        BlockDecl name block -> do
            recordID name -- block name is in scope for body
            checkBlock (nameType name) block
        Let name expr -> do
            checkExpression (nameType name) expr
            recordID name -- let name is not in scope for rhs
        Assign name value -> do
            checkID name
            checkValue (nameType name) value
        Say value -> do
            checkValue Text value
        Write value -> do
            checkValue Int value
    checkExpression expectedType expr = do
        checkType expectedType expr
        case expr of
            Value value -> do
                -- we already checked the type, we just want to check if it's in scope
                checkValue (typeOf expr) value
            UnaryOperator _ value -> do
                -- we abuse the fact that the unary ops have matching input and output types
                checkValue (typeOf expr) value
            BinaryOperator value1 op value2 -> do
                mapM_ (checkValue opInputType) [value1, value2] where
                    opInputType = case op of
                        ArithmeticOperator _ -> Int
                        ComparisonOperator _ -> Int
                        LogicalOperator    _ -> Bool
            Ask value -> do
                checkValue Text value
    checkValue expectedType value = do
        checkType expectedType (Value value)
        case value of
            Named   name -> checkID name
            Literal _    -> return ()
    checkTransfer = \case
        Jump target -> do
            checkTarget target
        Branch value targets -> do
            checkValue Bool value
            when (length targets != 2) $ do
                throwError (BadTargetCount targets)
            mapM_ checkTarget targets
    checkTarget target@Target { targetBlock, targetArgs } = do
        checkID targetBlock
        let expectedTypes = paramTypes (nameType targetBlock)
        when (length expectedTypes != length targetArgs) $ do
            throwError (BadArgsCount target)
        zipWithM_ checkValue expectedTypes targetArgs
    checkType expectedType node = do
        when (typeOf node != expectedType) $ do
            throwError (TypeMismatch expectedType node)
    recordID Name { ident, nameType } = do
        doModifyState $ \scope -> do
            when (memberID ident scope) $ do -- FIXME this should be a shallow check?
                throwError (Redefined ident)
            return (insertID ident nameType scope)
        return ()
    checkID Name { ident, nameType, description } = do
        inContext <- liftM (lookupID ident) getState
        case inContext of
            Nothing -> do
                throwError (NotInScope ident)
            Just recordedType -> do
                when (nameType != recordedType) $ do
                    throwError (Inconsistent (Name ident nameType description) (Name ident recordedType ""))




---------------------------------------------------------------------------------------------------- TRANSFORMS

eliminateTrivialBlocks :: Block -> Block
eliminateTrivialBlocks = evalState Map.empty . visitBlock where
    visitBlock Block { arguments, body, transfer } = do
        newBody     <- liftM catMaybes (mapM visitStatement body)
        newTransfer <- visitTransfer transfer
        return (Block arguments newBody newTransfer)
    visitStatement = \case
        BlockDecl name (Block [] [] (Jump target)) | targetBlock target != name -> do
            modifyState (Map.insert name target)
            return Nothing
        BlockDecl name nonTrivialBlock -> do
            newBlock <- visitBlock nonTrivialBlock
            return (Just (BlockDecl name newBlock))
        otherStatement -> do
            return (Just otherStatement)
    visitTransfer = \case
        Jump target -> do
            newTarget <- getAdjustedTarget target
            return (Jump newTarget)
        Branch value targets -> do
            newTargets <- mapM getAdjustedTarget targets
            return (Branch value newTargets)
    getAdjustedTarget oldTarget = do
        maybeNewTarget <- liftM (Map.lookup (targetBlock oldTarget)) getState
        case maybeNewTarget of
            Nothing -> do
                return oldTarget
            Just adjustedTarget -> do
                assertM (targetArgs oldTarget == []) -- if the block we're eliminating had arguments, it's not trivial!
                getAdjustedTarget adjustedTarget -- check if this block was _also_ trivial




---------------------------------------------------------------------------------------------------- PRETTY PRINTING

prettyType :: Type Expression -> P.Type
prettyType = \case
    Int  -> P.Int
    Bool -> P.Bool
    Text -> P.Text

builtin :: Text -> P.Document
builtin text = P.note (P.Identifier (P.IdentInfo text P.Use P.BuiltinName Nothing)) (P.pretty text)

blockId :: P.DefinitionOrUse -> Name Block -> P.Document
blockId defOrUse name = let info = P.IdentInfo (identText (ident name) ++ (if description name == "" then "" else "_" ++ description name)) defOrUse P.BlockName Nothing
                        in  P.note (P.Sigil info) "%" ++ P.note (P.Identifier info) (render (ident name) ++ P.pretty (if description name == "" then "" else "_" ++ description name))

-- TODO refactor `letID` and `blockId` maybe?
letId :: P.DefinitionOrUse -> Name Expression -> P.Document
letId   defOrUse name = let info = P.IdentInfo (identText (ident name)) defOrUse P.LetName (Just (prettyType (nameType name)))
                        in  P.note (P.Sigil info) "$" ++ P.note (P.Identifier info) (render (ident name))

identText :: ID node -> Text
identText = \case
    ASTName n -> AST.givenName n
    LetID   i -> showText i
    BlockID i -> showText i
    Return    -> "return" -- FIXME tag as P.Keyword!

renderBody :: [Statement] -> Transfer -> P.Document
renderBody statements transfer = mconcat (P.punctuate P.hardline (map render statements ++ [render transfer]))

-- we could probably refactor all these further but...

instance Render (ID node) where
    render = P.pretty . identText

instance Render (Type Expression) where
    render ty = P.note (P.Identifier (P.IdentInfo (showText ty) P.Use P.TypeName (Just (prettyType ty)))) (P.pretty (show ty))

instance Render (Name Expression) where
    render name = letId P.Definition name ++ P.colon ++ " " ++ render (nameType name)

instance Render Block where
    render block = renderBody (body block) (transfer block)

instance Render Statement where
    render = \case
        BlockDecl name block -> P.keyword "block" ++ " " ++ blockId P.Definition name ++ argumentList (arguments block) ++ " " ++ P.braces (P.nest 4 (P.hardline ++ renderBody (body block) (transfer block)) ++ P.hardline) where argumentList args = P.parens (P.hsep (P.punctuate "," (map render args)))
        Let       name expr  -> P.keyword "let"   ++ " " ++ render name ++ " " ++ P.defineEquals ++ " " ++ render expr
        Assign    name value -> letId P.Use name  ++ " " ++ P.assignEquals ++ " " ++ render value
        Say       value      -> builtin "say"     ++ P.parens (render value)
        Write     value      -> builtin "write"   ++ P.parens (render value)

instance Render Transfer where
    render = \case
        Jump         target  -> P.keyword "jump"   ++ " " ++ render target
        Branch value targets -> P.keyword "branch" ++ " " ++ render value ++ " " ++ P.hsep (map render targets)

instance Render Target where
    render target = blockId P.Use (targetBlock target) ++ P.parens (P.hsep (P.punctuate "," (map render (targetArgs target))))

instance Render Expression where
    render = \case
        Value          value            -> render value
        UnaryOperator  op value         -> P.unaryOperator op ++ render value
        BinaryOperator value1 op value2 -> render value1 ++ " " ++ P.binaryOperator op ++ " " ++ render value2
        Ask            value            -> builtin "ask" ++ P.parens (render value)

instance Render Value where
    render = \case
        Named   name    -> letId P.Use name
        Literal literal -> render literal

instance Render Literal where
    render = \case
        Number num  -> P.number num
        String text -> P.string text
