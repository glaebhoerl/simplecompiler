{-# LANGUAGE AllowAmbiguousTypes #-} -- for `idIsEven`

module IR (
    Type, BlockType (..), ID (..), NameWithType (..), Name, BlockName,
    Literal (..), Value (..), Expression (..), Statement (..), Block (..), Transfer (..), Target (..), Function (..),
    typeOf, typeOfBlock, translateFunction, validate, eliminateTrivialBlocks
) where

import MyPrelude

import qualified Data.Map as Map

import qualified Pretty as P
import qualified AST    as AST
import qualified Name   as Name
import qualified Type   as Type

import Pretty (Render, render)
import Type   (Type)


---------------------------------------------------------------------------------------------------- TYPE DEFINITIONS

newtype BlockType = BlockType {
    parameters :: [Type]
} deriving (Generic, Eq, Show)

data ID
    = ID      !Int
    | ASTName !Name.Name -- this includes all functions, as well as `return` and `break` points!
    deriving (Generic, Eq, Ord, Show)

data NameWithType nameType = Name {
    nameID      :: !ID,
    nameType    :: !nameType,
    description :: !Text
} deriving (Generic, Show, Functor)

type Name      = NameWithType Type
type BlockName = NameWithType BlockType

instance Eq  (NameWithType nameType) where
    (==) = (==) `on` nameID

instance Ord (NameWithType nameType) where
    compare = compare `on` nameID

data Literal
    = Number !Int64
    | String !Text
    | Unit
    deriving (Generic, Eq, Show)

data Value
    = Literal !Literal
    | Named   !Name
    deriving (Generic, Eq, Show)

data Expression
    = Value          !Value
    | UnaryOperator  !UnaryOperator !Value
    | BinaryOperator !Value !BinaryOperator !Value
    | Call           !Value ![Value]
    deriving (Generic, Eq, Show)

data Statement
    = BlockDecl !BlockName !Block
    | Let       !Name      !Expression -- also used for "expression statements" -- the name is simply ignored
    | Assign    !Name      !Value
    deriving (Generic, Eq, Show)

data Block = Block {
    arguments :: ![Name],
    body      :: ![Statement],
    transfer  :: !Transfer
} deriving (Generic, Eq, Show)

data Transfer
    = Jump   !Target
    | Branch !Value ![Target] -- targets are in "ascending order": false, then true
    deriving (Generic, Eq, Show)

data Target = Target {
    targetBlock :: !BlockName,
    targetArgs  :: ![Value]
} deriving (Generic, Eq, Show)

data Function = Function {
    functionID   :: !ID,
    functionBody :: !Block,
    returnBlock  :: !BlockName
} deriving (Generic, Eq, Show)

functionName :: Function -> Name
functionName Function { functionID, functionBody, returnBlock } =
    Name functionID (Type.Function argumentTypes returnType) ""
    where argumentTypes = map nameType (arguments functionBody)
          returnType    = assert (head (parameters (nameType returnBlock)))


typeOf :: Expression -> Type
typeOf = \case
    Value (Literal literal)    -> case literal of
        Number _               -> Type.Int
        String _               -> Type.Text
        Unit                   -> Type.Unit
    Value (Named name)         -> nameType name
    UnaryOperator Not      _   -> Type.Bool
    UnaryOperator Negate   _   -> Type.Int
    BinaryOperator _ op    _   -> case op of
        ArithmeticOperator _   -> Type.Int
        ComparisonOperator _   -> Type.Bool
        LogicalOperator    _   -> Type.Bool
    Call                   f _ -> case typeOf (Value f) of
        Type.Function      _ r -> r
        _                      -> bug "Call of non-function in IR"

typeOfBlock :: Block -> BlockType
typeOfBlock = BlockType . map nameType . arguments




---------------------------------------------------------------------------------------------------- TRANSLATION FRONTEND

class Monad m => TranslateM m where
    translateName      :: Type.TypedName -> m Name
    translateBlockName :: Type.TypedName -> m BlockName
    emitStatement      :: Statement -> m ()
    emitLet            :: Maybe Type.TypedName -> Expression -> m Name
    emitBlock          :: Text -> BlockType -> m Transfer -> m BlockName
    withContinuation   :: Either Type.TypedName (Text, BlockType) -> (BlockName -> m Transfer) -> m ()
    emitTransfer       :: Transfer -> m ()
    currentBlock       :: m BlockName
    currentArguments   :: m [Name]

translateTemporary :: TranslateM m => AST.Expression Type.TypedName -> m Value
translateTemporary = translateExpression Nothing

translateBinding :: TranslateM m => Type.TypedName -> AST.Expression Type.TypedName -> m Value
translateBinding = translateExpression . Just

translateExpression :: TranslateM m => Maybe Type.TypedName -> AST.Expression Type.TypedName -> m Value
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
        -- TODO use the provided name for the arg!
        withContinuation (Right ("join_" ++ opName, BlockType [Type.Bool])) $ \joinPoint -> do
            rhsBlock <- emitBlock opName (BlockType []) $ do
                value2 <- translateTemporary expr2
                return (Jump (Target joinPoint [value2]))
            let branches = case op of
                    And -> [Target joinPoint [value1], Target rhsBlock  []]
                    Or  -> [Target rhsBlock  [],       Target joinPoint [value1]]
            return (Branch value1 branches)
        args <- currentArguments -- (TODO this still works right?)
        return (Named (assert (head args)))
    AST.BinaryOperator expr1 op expr2 -> do
        value1 <- translateTemporary expr1
        value2 <- translateTemporary expr2
        name   <- emitNamedLet (BinaryOperator value1 op value2)
        return (Named name)
    AST.Call function exprs -> do
        fn     <- translateName function
        values <- mapM translateTemporary exprs
        name   <- emitNamedLet (Call (Named fn) values)
        return (Named name)

translateStatement :: TranslateM m => AST.Statement Type.TypedName -> m ()
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
        withContinuation (Right ("join_if", BlockType [])) $ \joinPoint -> do
            thenBlock <- emitBlock "if" (BlockType []) $ do
                translateStatements block
                return (Jump (Target joinPoint []))
            return (Branch value [Target joinPoint [], Target thenBlock []])
    AST.IfThenElse expr block1 block2 -> do
        value <- translateTemporary expr
        withContinuation (Right ("join_if_else", BlockType [])) $ \joinPoint -> do
            thenBlock <- emitBlock "if" (BlockType []) $ do
                translateStatements block1
                return (Jump (Target joinPoint []))
            elseBlock <- emitBlock "else" (BlockType []) $ do
                translateStatements block2
                return (Jump (Target joinPoint []))
            return (Branch value [Target elseBlock [], Target thenBlock []])
    AST.Forever block -> do
        withContinuation (Left (assert (AST.exitTarget block))) $ \_ -> do
            foreverBlock <- emitBlock "forever" (BlockType []) $ do
                blockBody <- currentBlock
                translateStatements block
                return (Jump (Target blockBody []))
            return (Jump (Target foreverBlock []))
    AST.While expr block -> do
        withContinuation (Left (assert (AST.exitTarget block))) $ \joinPoint -> do
            whileBlock <- emitBlock "while" (BlockType []) $ do
                conditionTest <- currentBlock
                blockBody <- emitBlock "while_body" (BlockType []) $ do
                    translateStatements block
                    return (Jump (Target conditionTest []))
                value <- translateTemporary expr
                return (Branch value [Target joinPoint [Literal Unit], Target blockBody []])
            return (Jump (Target whileBlock []))
    AST.Return target maybeExpr -> do
        translatedTarget <- translateBlockName target
        maybeValue       <- mapM translateTemporary maybeExpr
        emitTransfer (Jump (Target translatedTarget [fromMaybe (Literal Unit) maybeValue]))
    AST.Break target -> do
        translatedTarget <- translateBlockName target
        emitTransfer (Jump (Target translatedTarget []))
    AST.Expression expr -> do
        unused (translateTemporary expr)

translateStatements :: TranslateM m => AST.Block Type.TypedName -> m ()
translateStatements = mapM_ translateStatement . AST.statements


---------------------------------------------------------------------------------------------------- TRANSLATION BACKEND

translateFunction :: AST.Function Type.TypedName -> Function
translateFunction = evalState initialState . runTranslate . translate where
    initialState  = TranslateState  { lastID = 0, innermostBlock = BlockState (ID 0) "root" [] [] Nothing (Just (innermostBlock initialState)) } -- HACK
    translate AST.Function { AST.functionName, AST.arguments, AST.body = functionBody } = do
        -- TODO clean this whole thing up maybe somehow
        let exitTarget    = assert (AST.exitTarget functionBody)
        let rootBlockType = BlockType (map (Type.typeOf . AST.Named . AST.argumentName) arguments)
        returnBlock   <- translateBlockName exitTarget
        rootBlockID   <- newID BlockID
        rootBlockArgs <- mapM (translateName . AST.argumentName) arguments
        let rootBlockName = Name rootBlockID rootBlockType "root"
        setM (field @"innermostBlock" . field @"blockID") rootBlockID
        setM (field @"innermostBlock" . field @"blockDescription") "root"
        setM (field @"innermostBlock" . field @"blockArguments") rootBlockArgs
        translateStatements functionBody
        when (Type.typeOf (AST.Named exitTarget) == Type.Unit) $ do
            emitTransfer (Jump (Target returnBlock [Literal Unit]))
        getM (field @"innermostBlock") >>= (traceM . stringToText . take 10000 . show)
        -- FIXME HACK
        rootBlockDecl <- liftM (assert . match @"BlockDecl" . assert . head . statements . innermostBlock) getState
        assertEqM (fst rootBlockDecl) rootBlockName
        let functionBody = snd rootBlockDecl
        return Function { functionID = ASTName (Name.name functionName), functionBody, returnBlock }

newtype Translate a = Translate {
    runTranslate :: State TranslateState a
} deriving (Functor, Applicative, Monad, MonadState TranslateState)

data TranslateState = TranslateState {
    lastID         :: !Int,
    innermostBlock :: !BlockState
} deriving (Generic, Eq, Show)

data BlockState = BlockState {
    blockID           :: !ID,
    blockDescription  :: !Text,
    blockArguments    :: ![Name],
    statements        :: ![Statement],
    emittedTransfer   :: !(Maybe Transfer), -- this is `Just` iff we are currently processing the continuation of the block
    enclosingBlock    :: !(Maybe BlockState)
} deriving (Generic, Eq, Show)

data IDSort = LetID | BlockID

newID :: IDSort -> Translate ID
newID sort = do
    -- NOTE incrementing first is significant, ID 0 is the root block!
    modifyM (field @"lastID") $ \lastID ->
        let isEven = case sort of LetID -> True; BlockID -> False
        in lastID + (if isEven == (lastID % 2 == 0) then 2 else 1)
    new <- getM (field @"lastID")
    return (ID new)

newArgumentIDs :: BlockType -> Translate [Name]
newArgumentIDs (BlockType argTypes) = do
    forM argTypes $ \argType -> do
        argID <- newID LetID
        return (Name argID argType "")

instance TranslateM Translate where
    translateName :: Type.TypedName -> Translate Name -- TODO take these out of the type class?
    translateName (Name.NameWith name ty) = do
        return (Name (ASTName name) (translatedType ty) (Name.unqualifiedName name))
        where translatedType = \case
                Type.SmallType ty -> ty
                Type.Type         -> bug "Use of typename as local"

    translateBlockName :: Type.TypedName -> Translate BlockName
    translateBlockName (Name.NameWith name ty) = do
        -- TODO it's not clear in when we should copy the `unqualifiedName` as the `description` and when not...?
        -- right now it's inconsistent between lets and blocks
        return (Name (ASTName name) (translatedType ty) "")
        where translatedType = \case
                Type.SmallType ty -> BlockType [ty]
                Type.Type         -> bug "Use of typename as exit target"

    emitStatement :: Statement -> Translate ()
    emitStatement statement = do
        modifyM (field @"innermostBlock" . field @"statements") (++ [statement])
        return ()

    emitLet :: Maybe Type.TypedName -> Expression -> Translate Name
    emitLet providedName expr = do
        name <- case providedName of
            Just astName -> do
                translatedName <- translateName astName
                assertEqM (nameType translatedName) (typeOf expr)
                return translatedName
            Nothing -> do
                letID <- newID LetID
                return (Name letID (typeOf expr) "")
        emitStatement (Let name expr)
        return name

    emitBlock :: Text -> BlockType -> Translate Transfer -> Translate BlockName
    emitBlock description argTypes translateBody = do
        blockID <- newID BlockID
        args    <- newArgumentIDs argTypes
        modifyM (field @"innermostBlock") (\previouslyInnermost -> BlockState blockID description args [] Nothing (Just previouslyInnermost))
        blockName <- currentBlock
        transferAtEnd <- translateBody
        emitTransfer transferAtEnd -- this will handle popping and emitting the finished block
        return blockName

    withContinuation :: Either Type.TypedName (Text, BlockType) -> (BlockName -> Translate Transfer) -> Translate ()
    withContinuation blockSpec inBetweenCode = do
        alreadyEmitted <- getM (field @"innermostBlock" . field @"emittedTransfer")
        assertEqM alreadyEmitted Nothing
        nextBlockName <- case blockSpec of
            Left nextBlockAstName -> do
                translateBlockName nextBlockAstName
            Right (nextBlockDescription, nextBlockParams) -> do
                nextBlockID <- newID BlockID
                return (Name nextBlockID nextBlockParams nextBlockDescription)
        nextBlockArgs <- newArgumentIDs (nameType nextBlockName)
        let nextBlockStub = Block { arguments = nextBlockArgs, body = [], transfer = Jump (Target (Name (ID -1) (BlockType []) "") []) }
        emitStatement (BlockDecl nextBlockName nextBlockStub)
        transfer <- inBetweenCode nextBlockName -- TODO if any `emitTransfer` is done in here, it's a `bug`!
        setM (field @"innermostBlock" . field @"emittedTransfer") (Just transfer)
        modifyM (field @"innermostBlock") (\previouslyInnermost -> BlockState (nameID nextBlockName) (description nextBlockName) nextBlockArgs [] Nothing (Just previouslyInnermost))
        return ()

    -- does this black have an emitted continuation?
    --   yes -> push a new block with this block as the parent block, with the name/args of the emitted continuation
    --   no  -> is this block the continuation of the enclosing block?
    --      yes -> pop this block, then replace the continuation stub in the parent block with this block
    --               then recurse from "is this block the continuation...?" for the parent block
    --      no  -> pop this block, append this block as a blockdecl statement to the parent block
    emitTransfer :: Transfer -> Translate ()
    emitTransfer transfer = do
        BlockState { blockArguments, statements, emittedTransfer } <- getM (field @"innermostBlock")
        assertEqM emittedTransfer Nothing
        blockName <- currentBlock
        let finishedBlock = Block blockArguments statements transfer
        modifyM (field @"innermostBlock") (assert . enclosingBlock)
        parentEmittedTransfer <- getM (field @"innermostBlock" . field @"emittedTransfer")
        case parentEmittedTransfer of
            Just parentTransfer -> do
                modifyM (field @"innermostBlock" . field @"statements") $ map $ \case
                    BlockDecl name _ | name == blockName -> BlockDecl blockName finishedBlock
                    otherStatement                       -> otherStatement
                setM (field @"innermostBlock" . field @"emittedTransfer") Nothing
                emitTransfer parentTransfer
            _ -> do
                emitStatement (BlockDecl blockName finishedBlock)

    currentBlock :: Translate BlockName
    currentBlock = do
        blockID     <- getM (field @"innermostBlock" . field @"blockID")
        description <- getM (field @"innermostBlock" . field @"blockDescription")
        arguments   <- currentArguments
        return (Name blockID (BlockType (map nameType arguments)) description)

    currentArguments :: Translate [Name]
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

-- TODO think through what other new error possibilities there might be!
data ValidationError
    = NotInScope         !ID
    | Redefined          !ID
    | ExpectedValue      !BlockName
    | ExpectedBlock      !Name
    | Inconsistent       !Name      !Name
    | BlockInconsistent  !BlockName !BlockName
    | TypeMismatch       !Type      !Expression
    | BlockTypeMismatch  !BlockType !Block
    | BadTargetCount     !Transfer
    | BadTargetArgsCount !Target
    | BadCallArgsCount   !Expression
    | CallOfNonFunction  !Expression
    deriving (Generic, Show)

validate :: [Function] -> Either ValidationError ()
validate = runExcept . evalStateT [Map.empty] . mapM_ checkFunction where
    checkFunction function@Function { functionBody, returnBlock } = do
        recordName (functionName function)
        recordBlockName returnBlock
        checkBlock functionBody
    checkBlock block = do
        modifyState (prepend Map.empty)
        mapM_ recordName     (arguments block)
        mapM_ checkStatement (body      block)
        checkTransfer        (transfer  block)
        modifyState (assert . tail)
        return ()
    checkStatement = \case
        BlockDecl name block -> do
            recordBlockName name -- block name is in scope for body
            checkBlockType (nameType name) block
            checkBlock block
        Let name expr -> do
            checkExpression (nameType name) expr
            recordName name -- let name is not in scope for rhs
        Assign name value -> do
            checkName name
            checkValue (nameType name) value
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
                        ArithmeticOperator _ -> Type.Int
                        ComparisonOperator _ -> Type.Int
                        LogicalOperator    _ -> Type.Bool
            Call fn args -> case typeOf (Value fn) of
                Type.Function argTypes returnType -> do
                    mapM_ checkName (match @"Named" fn)
                    when (returnType != expectedType) $ do
                        throwError (TypeMismatch expectedType expr)
                    when (length args != length argTypes) $ do
                        throwError (BadCallArgsCount expr)
                    zipWithM_ checkValue argTypes args
                _ -> do
                    throwError (CallOfNonFunction expr)
    checkValue expectedType value = do
        checkType expectedType (Value value)
        mapM_ checkName (match @"Named" value)
    checkTransfer = \case
        Jump target -> do
            checkTarget target
        Branch value targets -> do
            checkValue Type.Bool value
            when (length targets != 2) $ do
                throwError (BadTargetCount (Branch value targets))
            mapM_ checkTarget targets
    checkTarget target@Target { targetBlock, targetArgs } = do
        checkBlockName targetBlock
        let expectedTypes = parameters (nameType targetBlock)
        when (length expectedTypes != length targetArgs) $ do
            throwError (BadTargetArgsCount target)
        zipWithM_ checkValue expectedTypes targetArgs
    checkType expectedType expr = do
        when (typeOf expr != expectedType) $ do
            throwError (TypeMismatch expectedType expr)
    checkBlockType expectedType block = do
        when (typeOfBlock block != expectedType) $ do
            throwError (BlockTypeMismatch expectedType block)
    checkName name@Name { nameID, nameType, description } = do
        when (nameID `is` (constructor @"ASTName" . constructor @"BuiltinName")) $ do  -- FIXME HACK
            recordName name
        recordedType <- lookupType nameID
        when (nameType != recordedType) $ do
            throwError (Inconsistent (Name nameID nameType description) (Name nameID recordedType ""))
    checkBlockName Name { nameID, nameType, description } = do -- FIXME deduplicate
        recordedType <- lookupBlockType nameID
        when (nameType != recordedType) $ do
            throwError (BlockInconsistent (Name nameID nameType description) (Name nameID recordedType ""))
    lookupType nameID = do
        nameType <- lookupID nameID
        case nameType of
            Right valueType -> return valueType
            Left  blockType -> throwError (ExpectedValue (Name nameID blockType ""))
    lookupBlockType nameID = do
        nameType <- lookupID nameID
        case nameType of
            Left  blockType -> return blockType
            Right valueType -> throwError (ExpectedBlock (Name nameID valueType ""))
    lookupID nameID = do
        scopes <- getState
        case Map.lookup nameID (Map.unions scopes) of
            Just nameType -> return nameType
            Nothing       -> throwError (NotInScope nameID)
    recordName      = insertName . fmap Right
    recordBlockName = insertName . fmap Left
    insertName Name { nameID, nameType } = do
        doModifyState $ \(names : parents) -> do
            when (Map.member nameID names) $ do -- FIXME this should be a shallow check?
                throwError (Redefined nameID)
            return (Map.insert nameID nameType names : parents)
        return ()



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
                assertEqM (targetArgs oldTarget) [] -- if the block we're eliminating had arguments, it's not trivial!
                getAdjustedTarget adjustedTarget -- check if this block was _also_ trivial




---------------------------------------------------------------------------------------------------- PRETTY PRINTING

prettyType :: Type -> P.Type
prettyType = \case
    Type.Int          -> P.Int
    Type.Bool         -> P.Bool
    Type.Text         -> P.Text
    Type.Unit         -> P.Unit
    Type.Function _ _ -> P.Function

blockId :: P.DefinitionOrUse -> BlockName -> P.Document
blockId defOrUse name = let info = P.IdentInfo (identText (nameID name) ++ (if description name == "" then "" else "_" ++ description name)) defOrUse P.Block False
                        in  P.note (P.Sigil info) "%" ++ P.note (P.Identifier info) (render (nameID name) ++ P.pretty (if description name == "" then "" else "_" ++ description name))

-- TODO refactor `letID` and `blockId` maybe?
letId :: P.DefinitionOrUse -> Name -> P.Document
letId   defOrUse name = let info = P.IdentInfo (identText (nameID name)) defOrUse (prettyType (nameType name)) False
                        in  P.note (P.Sigil info) "$" ++ P.note (P.Identifier info) (render (nameID name))

identText :: ID -> Text
identText = \case
    ASTName n -> Name.unqualifiedName n
    ID      i -> showText i

renderBody :: [Statement] -> Transfer -> P.Document
renderBody statements transfer = P.hardline ++ P.braces (P.nest 4 (P.hardline ++ mconcat (P.punctuate P.hardline (map render statements ++ [render transfer]))) ++ P.hardline)

argumentList :: [Name] -> P.Document
argumentList = P.parens . P.hsep . P.punctuate "," . map render

-- we could probably refactor all these further but...

instance Render ID where
    render = P.pretty . identText

-- NOTE `instance Render Type` is provided by `module Type`

instance Render Name where
    render name = letId P.Definition name ++ P.colon ++ " " ++ render (nameType name)

instance Render [Function] where
    render = mconcat . P.punctuate P.hardline . map render

instance Render Function where
    render function@Function { functionBody, returnBlock } =
        P.keyword "function" ++ " " ++ letId P.Definition (functionName function) ++ argumentList (arguments functionBody) ++ " " ++ P.keyword "returns" ++ " " ++ render returnType ++
         renderBody (body functionBody) (transfer functionBody)
        where returnType = assert (head (parameters (nameType returnBlock)))

instance Render Block where
    render block = renderBody (body block) (transfer block)

instance Render Statement where
    render = \case
        BlockDecl name block -> P.keyword "block" ++ " " ++ blockId P.Definition name ++ argumentList (arguments block) ++ renderBody (body block) (transfer block)
        Let       name expr  -> P.keyword "let"   ++ " " ++ render name ++ " " ++ P.defineEquals ++ " " ++ render expr
        Assign    name value -> letId P.Use name  ++ " " ++ P.assignEquals ++ " " ++ render value

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
        Call           fn args          -> render fn ++ P.parens (P.hsep (P.punctuate "," (map render args)))

instance Render Value where
    render = \case
        Named   name    -> letId P.Use name
        Literal literal -> render literal

instance Render Literal where
    render = \case
        Number num  -> P.number num
        String text -> P.string text
        Unit        -> P.note (P.Literal P.Unit) "Unit"
