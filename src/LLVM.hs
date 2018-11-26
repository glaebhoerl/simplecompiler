-- we want to use `functionDefaults` and `globalVariableDefaults` and so on without warnings
-- (maybe it would be better if llvm-hs split these out into their own types so the record updates
-- would not be incomplete, but ¯\_(ツ)_/¯)
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module LLVM (translateFunctions, Module) where

import MyPrelude

import Data.List (transpose)

import qualified Data.Char

import qualified Data.Map.Strict as Map

import qualified LLVM.AST                   as L
import qualified LLVM.AST.CallingConvention as L
import qualified LLVM.AST.Constant          as LC
import qualified LLVM.AST.Global            as LG
import qualified LLVM.AST.IntegerPredicate  as L

import LLVM.AST          (Module, Definition, Global, Parameter (Parameter), BasicBlock (BasicBlock),
                          Instruction, Terminator, Named (Do, (:=)), Operand (LocalReference, ConstantOperand))
import LLVM.AST.Constant (Constant (GlobalReference))
import LLVM.AST.Type     (i1, i8, i32, i64, ptr, void)

import qualified Pretty
import qualified Name
import qualified Type
import qualified IR



---------------------------------------------------------------------------------------------------- TRANSLATION FRONTEND

instance Pretty.Render Module where
    render = Pretty.pretty . showText
    outputWithStyle _ handle = hPutStr handle . showText

translateFunctions :: [IR.Function] -> Module
translateFunctions functions = result where
    result      = L.defaultModule { L.moduleDefinitions = definitions }
    definitions = map L.GlobalDefinition (fst (runTwoPass generate))
    generate :: LLVM m => m ()
    generate = do
        emitGlobal externPrintf
        emitGlobal externScanf
        emitBlock "return" do
            let returnArg = IR.Name (IR.ID 0) Type.Int ""
            translateArguments [returnArg]
            returnValue64 <- load returnArg
            let instr = L.Trunc {
                L.operand0 = returnValue64,
                L.type'    = i32,
                L.metadata = []
            }
            returnName32 <- freshName
            emit (returnName32 := instr)
            let returnValue32 = LocalReference i32 returnName32
            return (L.Ret { L.returnOperand = Just returnValue32, L.metadata' = [] }, [])
        emitBlock "start" do -- NOTE the name "start" is significant (to `runTwoPass`)
            let block = todo
            translateBlock block
    externPrintf = L.functionDefaults {
        LG.name        = "printf",
        LG.parameters  = ([Parameter (ptr i8) "" []], True),
        LG.returnType  = i32
    }
    externScanf = L.functionDefaults {
        LG.name        = "scanf",
        LG.parameters  = ([Parameter (ptr i8) "" []], True),
        LG.returnType  = i32
    }

class Monad m => LLVM m where
    emitBlock    :: L.Name -> m (Terminator, [CallsBlockWith]) -> m ()
    getArguments :: m [CalledByBlockWith]
    emit         :: Named Instruction -> m ()
    emitAlloca   :: L.Type -> L.Name -> m ()
    emitGlobal   :: Global -> m ()
    freshName    :: m L.Name

data CallsBlockWith = CallsBlockWith {
    calledBlock     :: L.Name,
    argumentsPassed :: [Operand]
} deriving (Generic, Eq, Show)

data CalledByBlockWith = CalledByBlockWith {
    callingBlock      :: L.Name,
    argumentsReceived :: [Operand]
} deriving (Generic, Eq, Show)

translatedType :: IR.Type -> L.Type
translatedType = \case
    Type.Bool -> i1
    Type.Int  -> i64
    Type.Text -> ptr i8
    Type.Unit -> void
    Type.Function argTypes returnType ->
        ptr (L.FunctionType (translatedType returnType) (map translatedType (filter (!= Type.Unit) argTypes)) False)

allocaForLet :: IR.Name -> Operand
allocaForLet (IR.Name ident nameType _) = LocalReference (ptr (translatedType nameType)) (translatedID ident)

translatedID :: IR.ID -> L.Name
translatedID = \case
    IR.ID      num  -> L.mkName (show num)
    IR.ASTName name -> L.mkName (textToString (Name.qualifiedName name))

alloca :: LLVM m => IR.Name -> m ()
alloca (IR.Name ident nameType _) = emitAlloca (translatedType nameType) (translatedID ident)

load :: LLVM m => IR.Name -> m Operand
load name = do
    newName <- freshName
    let instr = L.Load {
        L.volatile       = False,
        L.address        = allocaForLet name,
        L.maybeAtomicity = Nothing,
        L.alignment      = 0,
        L.metadata       = []
    }
    emit (newName := instr)
    return (LocalReference (translatedType (IR.nameType name)) newName)

store :: LLVM m => IR.Name -> Operand -> m ()
store name operand = do
    let instr = L.Store {
        L.volatile       = False,
        L.address        = allocaForLet name,
        L.value          = operand,
        L.maybeAtomicity = Nothing,
        L.alignment      = 0,
        L.metadata       = []
    }
    emit (Do instr)

translateValue :: LLVM m => IR.Value -> m Operand
translateValue = \case
    IR.Literal literal -> case literal of
        IR.Number num -> do
            return (number (Bits 64) (fromIntegral num))
        IR.String text -> do
            translateStringLiteral text
    IR.Named name -> do
        load name

newtype Bits = Bits Word32

number :: Bits -> Integer -> Operand
number (Bits bits) value = ConstantOperand (LC.Int { LC.integerBits = bits, LC.integerValue = value })

translateUnaryOp :: Operand -> UnaryOperator -> Instruction
translateUnaryOp operand = \case
    Not    -> L.Xor { L.operand0 = number (Bits 1)  1, L.operand1 = operand, L.metadata = [] }
    Negate -> L.Sub { L.operand0 = number (Bits 64) 0, L.operand1 = operand, L.metadata = [], L.nsw = False, L.nuw = False }

translateBinaryOp :: Operand -> Operand -> BinaryOperator -> Instruction
translateBinaryOp operand0 operand1 = \case
    -- TODO: Handle the UB cases :(
    ArithmeticOperator Add -> L.Add  { L.operand0, L.operand1, L.metadata = [], L.nsw   = False, L.nuw = False }
    ArithmeticOperator Sub -> L.Sub  { L.operand0, L.operand1, L.metadata = [], L.nsw   = False, L.nuw = False }
    ArithmeticOperator Mul -> L.Add  { L.operand0, L.operand1, L.metadata = [], L.nsw   = False, L.nuw = False }
    ArithmeticOperator Div -> L.SDiv { L.operand0, L.operand1, L.metadata = [], L.exact = False }
    ArithmeticOperator Mod -> L.SRem { L.operand0, L.operand1, L.metadata = [] } -- TODO: remainder vs. modulo semantics?
    LogicalOperator    And -> L.And  { L.operand0, L.operand1, L.metadata = [] }
    LogicalOperator    Or  -> L.Or   { L.operand0, L.operand1, L.metadata = [] }
    ComparisonOperator op  -> L.ICmp { L.operand0, L.operand1, L.metadata = [], L.iPredicate = predicateFor op }
    where predicateFor = \case
            Less           -> L.SLT
            LessEqual      -> L.SLE
            Greater        -> L.SGT
            GreaterEqual   -> L.SGE
            Equal          -> L.EQ
            NotEqual       -> L.NE

translateExpression :: LLVM m => IR.Expression -> m Operand
translateExpression expr = let localRef = LocalReference (translatedType (IR.typeOf expr)) in case expr of
    IR.Value value -> do
        translateValue value
    IR.UnaryOperator op value -> do
        operand <- translateValue value
        newName <- freshName
        emit (newName := translateUnaryOp operand op)
        return (localRef newName)
    IR.BinaryOperator value1 op value2 -> do
        operand1 <- translateValue value1
        operand2 <- translateValue value2
        newName  <- freshName
        emit (newName := translateBinaryOp operand1 operand2 op)
        return (localRef newName)
    IR.Call fn args -> do
        let value = todo
        operand     <- translateValue value
        printFormat <- translateStringLiteral "%s "
        emit (Do (call printf [printFormat, operand]))

        scanFormat  <- translateStringLiteral "%26lld"
        let instr = L.Alloca { -- TODO factor this out from `emitAlloca`
            L.allocatedType = i64,
            L.numElements   = Nothing,
            L.alignment     = 0,
            L.metadata      = []
        }
        outputName <- freshName
        emit (outputName := instr)
        let outputPtr = LocalReference (ptr i64) outputName
        emit (Do (call scanf [scanFormat, outputPtr]))

        loadedName <- freshName
        let loadInstr = L.Load { -- TODO factor this out from `load`
            L.volatile       = False,
            L.address        = outputPtr,
            L.maybeAtomicity = Nothing,
            L.alignment      = 0,
            L.metadata       = []
        }
        emit (loadedName := loadInstr)
        return (LocalReference i64 loadedName)

translateStringLiteral :: LLVM m => Text -> m Operand
translateStringLiteral text = do
    globalName <- freshName
    let (type', global) = stringConstant globalName text
    emitGlobal global
    ptrName <- freshName
    -- TODO: Should we use a Constant GetElementPtr instead?
    --   No we shouldn't: only the string itself needs to be part of the binary, not the pointer to it.
    -- If yes, can we give it the string constant directly, or do we need 2 separate globals?
    --   We'd need two separate ones: it expects a constant *pointer* as argument (not an array).
    let instr = L.GetElementPtr {
        L.address  = ConstantOperand (GlobalReference (ptr type') globalName),
        L.indices  = replicate 2 (number (Bits 32) 0),
        L.inBounds = False,
        L.metadata = []
    }
    emit (ptrName := instr)
    return (LocalReference (ptr i8) ptrName)

stringConstant :: L.Name -> Text -> (L.Type, Global)
stringConstant name text = (type', globalConstant name type' constant) where
    type'    = L.ArrayType (fromIntegral (length charList)) i8
    constant = (LC.Array i8 . map (LC.Int 8 . fromIntegral . Data.Char.ord)) charList
    charList = textToString text ++ [Data.Char.chr 0]

globalConstant :: L.Name -> L.Type -> Constant -> Global
globalConstant name type' value =
    LG.globalVariableDefaults {
        LG.name        = name,
        LG.initializer = Just value,
        LG.isConstant  = True,
        LG.type'       = type'
    }

translateStatement :: LLVM m => IR.Statement -> m ()
translateStatement = \case
    IR.BlockDecl (IR.Name ident _ _) body -> do
        emitBlock (translatedID ident) do
            translateBlock body
    IR.Let name expr -> do
        alloca name
        operand <- translateExpression expr
        store name operand
    IR.Assign name value -> do
        operand <- translateValue value
        store name operand
    {-
    IR.Say value -> do
        formatPtr <- translateStringLiteral "%s\n"
        operand   <- translateValue value
        emit (Do (call printf [formatPtr, operand]))
    IR.Write value -> do
        formatPtr <- translateStringLiteral "%lld\n"
        operand   <- translateValue value
        emit (Do (call printf [formatPtr, operand]))
    -}

call :: Operand -> [Operand] -> Instruction
call callee args = L.Call {
    L.tailCallKind       = Nothing,
    L.callingConvention  = L.C,
    L.returnAttributes   = [],
    L.function           = Right callee,
    L.arguments          = map (\arg -> (arg, [])) args,
    L.functionAttributes = [],
    L.metadata           = []
}

printf :: Operand
printf = ConstantOperand (GlobalReference (ptr (L.FunctionType i32 [ptr i8] True)) "printf")

scanf :: Operand
scanf = ConstantOperand (GlobalReference (ptr (L.FunctionType i32 [ptr i8] True)) "scanf")

translateBlock :: LLVM m => IR.Block -> m (Terminator, [CallsBlockWith])
translateBlock IR.Block { IR.arguments, IR.body, IR.transfer } = do
    translateArguments arguments
    mapM_ translateStatement body
    translateTransfer transfer

translateArguments :: LLVM m => [IR.Name] -> m ()
translateArguments arguments = do
    -- [For each calling block: CalledByBlockWith { callingBlock = block's name, argumentsReceived = [values it passed for arguments] }]
    calledByBlocks <- getArguments
    forM_ calledByBlocks \CalledByBlockWith { argumentsReceived } -> do
        assertEqM (length argumentsReceived) (length arguments)
    -- [For each calling block: [For each argument it passed: (the argument's value, the block's name)]]
    let incomingValuesGroupedByBlock =
            map (\CalledByBlockWith { callingBlock, argumentsReceived } ->
                    map (\operand -> (operand, callingBlock))
                        argumentsReceived)
                calledByBlocks
    -- [For each argument: [For each calling block: (the argument's value, the block's name)]]
    let incomingValuesGroupedByArg = transpose incomingValuesGroupedByBlock
    -- `++ repeat []` so we process all the arguments even if this block is never called (which is always the case in the FirstPass!!)
    forM_ (zip arguments (incomingValuesGroupedByArg ++ repeat [])) \(argument, incomingValues) -> do
        phiName <- freshName
        when (incomingValues != []) do
            let instr = L.Phi {
                L.type'          = translatedType (IR.nameType argument),
                L.incomingValues = incomingValues,
                L.metadata       = []
            }
            emit (phiName := instr)
            -- HACK: We make an alloca for each argument, despite them never being mutated or anything,
            -- just so we can refer to them consistently using the `allocaForLet` mapped names, just like normal `let`s.
            alloca argument
            store argument (LocalReference (translatedType (IR.nameType argument)) phiName)

translateTransfer :: LLVM m => IR.Transfer -> m (Terminator, [CallsBlockWith])
translateTransfer = \case
    IR.Jump target -> do
        callsBlockWith <- translateTarget target
        let instr = L.Br { L.dest = calledBlock callsBlockWith, L.metadata' = [] }
        return (instr, [callsBlockWith])
    IR.Branch value targets -> do
        operand <- translateValue value
        assertEqM (length targets) 2
        callsBlocksWith <- mapM translateTarget targets
        let instr = L.CondBr {
            L.condition = operand,
            L.trueDest  = calledBlock (callsBlocksWith !! 1),
            L.falseDest = calledBlock (callsBlocksWith !! 0),
            L.metadata' = []
        }
        return (instr, callsBlocksWith)

translateTarget :: LLVM m => IR.Target -> m CallsBlockWith
translateTarget IR.Target { IR.targetBlock, IR.targetArgs } = do
    operands <- mapM translateValue targetArgs
    return CallsBlockWith { calledBlock = translatedID (IR.nameID targetBlock), argumentsPassed = operands }



---------------------------------------------------------------------------------------------------- TRANSLATION BACKEND

-- Two passes using plain state monads

runTwoPass :: (forall m. LLVM m => m a) -> ([Global], a)
runTwoPass generate = result where
    firstResult  = execState (FirstState 0 Map.empty)                                     (runFirstPass  generate)
    secondResult = runState  (SecondState 0 (callersMap firstResult) [] [] [] dummyBlock) (runSecondPass generate)
    dummyBlock   = UnfinishedBlock (L.UnName maxBound) [] Nothing
    resultValue  = fst secondResult
    SecondState {
        globals,
        allocas,
        finishedBlocks,
        unfinishedBlocks
    }            = snd secondResult
    allocaBlock  = BasicBlock "alloca" allocas (Do (L.Br { L.dest = "start", L.metadata' = [] }))
    createdFn    = LG.functionDefaults { LG.name = "main", LG.returnType = i32, LG.basicBlocks = allocaBlock : finishedBlocks }
    result       = if unfinishedBlocks == dummyBlock then (createdFn : globals, resultValue) else bug "The dummy block changed somehow!"

newtype FirstPass a = FirstPass {
    runFirstPass :: State FirstState a
} deriving (Functor, Applicative, Monad, MonadState FirstState)

data FirstState = FirstState {
    freshNamer :: Word,
    callersMap :: Map L.Name [CalledByBlockWith]
} deriving Generic

instance LLVM FirstPass where
    freshName = do
        field @"freshNamer" += 1
        num <- getM (field @"freshNamer")
        return (L.UnName num)
    emitBlock blockName bodyAction = do
        (_, callsBlocksWith) <- bodyAction
        forM_ callsBlocksWith \CallsBlockWith { calledBlock, argumentsPassed } -> do
            when (argumentsPassed != []) do
                let calledByThisBlock = CalledByBlockWith { callingBlock = blockName, argumentsReceived = argumentsPassed }
                modifyM (field @"callersMap") (Map.alter (Just . prepend calledByThisBlock . fromMaybe []) calledBlock)
                return ()
    emit       _   = return ()
    emitAlloca _ _ = return ()
    emitGlobal _   = return ()
    getArguments   = return []

newtype SecondPass a = SecondPass {
    runSecondPass :: State SecondState a
} deriving (Functor, Applicative, Monad, MonadState SecondState)

data SecondState = SecondState {
    secondNamer      :: Word,
    callersOfBlocks  :: Map L.Name [CalledByBlockWith], -- readonly
    allocas          :: [Named Instruction],
    globals          :: [Global],
    finishedBlocks   :: [BasicBlock],
    unfinishedBlocks :: UnfinishedBlock
} deriving Generic

data UnfinishedBlock = UnfinishedBlock {
    blockName     :: L.Name,
    instructions  :: [Named Instruction],
    previousBlock :: Maybe UnfinishedBlock
} deriving (Generic, Eq)

instance LLVM SecondPass where
    freshName = do
        field @"secondNamer" += 1
        num <- getM (field @"secondNamer")
        return (L.UnName num)
    emit instruction = do
        modifyM (field @"unfinishedBlocks" . field @"instructions") (++ [instruction])
        return ()
    emitAlloca type' name = do
        let instr = L.Alloca {
            L.allocatedType = type',
            L.numElements   = Nothing,
            L.alignment     = 0,
            L.metadata      = []
        }
        modifyM (field @"allocas") (++ [name := instr])
        return ()
    emitGlobal global = do
        modifyM (field @"globals") (prepend global)
        return ()
    getArguments = do
        thisBlock <- getM (field @"unfinishedBlocks" . field @"blockName")
        callers   <- getM (field @"callersOfBlocks")
        return (Map.findWithDefault [] thisBlock callers)
    emitBlock blockName bodyAction = do
        modifyM (field @"unfinishedBlocks") (UnfinishedBlock blockName [] . Just)
        (terminator, callsBlocksWith) <- bodyAction
        -- here we just assert that the `callsBlocksWith` is the same as what we got in the first pass
        forM_ callsBlocksWith \CallsBlockWith { calledBlock, argumentsPassed } -> do
            -- TODO assert that the `calledBlock` is one of those in `terminator`
            savedCallers <- liftM (Map.findWithDefault [] calledBlock) (getM (field @"callersOfBlocks"))
            let calledByUs = filter (\CalledByBlockWith { callingBlock } -> callingBlock == blockName) savedCallers
            -- TODO there is a probably a simpler way to express this?
            if argumentsPassed == []
                then do
                    assertEqM calledByUs []
                else do
                    assertEqM calledByUs [CalledByBlockWith { callingBlock = blockName, argumentsReceived = argumentsPassed }]
        doModifyM (field @"finishedBlocks") \finishedBlocks -> do
            instructions <- getM (field @"unfinishedBlocks" . field @"instructions")
            savedName    <- getM (field @"unfinishedBlocks" . field @"blockName")
            assertEqM blockName savedName
            let newBlock = BasicBlock savedName instructions (Do terminator)
            return (newBlock : finishedBlocks)
        modifyM (field @"unfinishedBlocks" ) (assert . previousBlock)
        return ()
