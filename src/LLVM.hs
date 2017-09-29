module LLVM (translate, Module) where

import MyPrelude

import Data.List (transpose, nub)

import qualified Data.Char

import qualified Data.Map.Strict as Map

import qualified LLVM.AST                   as L
import qualified LLVM.AST.CallingConvention as LCC
import qualified LLVM.AST.Constant          as LC
import qualified LLVM.AST.Global            as LG
import qualified LLVM.AST.IntegerPredicate  as L

import LLVM.AST          (Module, Definition, Global, Parameter (Parameter), BasicBlock (BasicBlock), Instruction, Terminator, Named (Do, (:=)), Operand (LocalReference, ConstantOperand))
import LLVM.AST.Constant (Constant (GlobalReference))
import LLVM.AST.Type     (i1, i8, i32, i64, ptr)

import qualified Name
import qualified IR

translate :: IR.Block -> Module
translate block = result where
    result      = L.defaultModule { L.moduleDefinitions = definitions }
    definitions = (map L.GlobalDefinition . fst . runLLVM) generate
    generate    = do
        emitGlobal externPrintf
        emitGlobal externScanf
        emitBlock "return" $ do
            let returnArg = IR.Name (IR.LetID 0) IR.Int ""
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
        emitBlock "start" $ do -- currently this will become the first basic block, as LLVM requires, because `emitBlock` prepends!
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
    emitGlobal   :: Global -> m ()
    freshName    :: m L.Name

data CallsBlockWith = CallsBlockWith {
    calledBlock     :: !L.Name,
    argumentsPassed :: ![Operand]
} deriving Generic

data CalledByBlockWith = CalledByBlockWith {
    callingBlock      :: !L.Name,
    argumentsReceived :: ![Operand]
} deriving Generic

-- TODO name
newtype M a = M {
    runM :: Tardis Backwards Forwards a
} deriving (Functor, Applicative, Monad, MonadFix, MonadState Forwards, MonadTardis Backwards Forwards)

type Backwards = Map L.Name [CalledByBlockWith]

data Forwards = Forwards {
    unNamer          :: !Word,
    globals          :: ![Global],
    finishedBlocks   :: ![BasicBlock],
    unfinishedBlocks :: !UnfinishedBlock
} deriving Generic

data UnfinishedBlock = UnfinishedBlock {
    blockName     :: !L.Name,
    instructions  :: ![Named Instruction],
    previousBlock :: !(Maybe UnfinishedBlock)
} deriving (Generic, Eq)

runLLVM :: M a -> ([Global], a)
runLLVM = getOutput . runTardis (Map.empty, (Forwards 0 [] [] dummyBlock)) . runM where
    dummyBlock = UnfinishedBlock (L.UnName -1) [] Nothing
    getOutput (a, (_, Forwards { globals, finishedBlocks, unfinishedBlocks })) =
        if unfinishedBlocks == dummyBlock
            then (createdFunction : globals, a)
            else bug "The dummy block changed somehow!"
                where createdFunction = LG.functionDefaults { LG.name = "main", LG.returnType = i32, LG.basicBlocks = finishedBlocks }

instance LLVM M where
    freshName = do
        field @"unNamer" += 1
        num <- getM (field @"unNamer")
        return (L.UnName num)
    emit instr = do
        modifyM (field @"unfinishedBlocks" . field @"instructions") (++ [instr])
    emitGlobal global = do
        modifyM (field @"globals") (prepend global)
    getArguments = do
        thisBlock <- getM (field @"unfinishedBlocks" . field @"blockName")
        callees   <- backGetState
        return (Map.findWithDefault [] thisBlock callees)
    emitBlock blockName bodyAction = do
        modifyM (field @"unfinishedBlocks") (UnfinishedBlock blockName [] . Just)
        (terminator, callsBlocksWith) <- bodyAction
        forM_ callsBlocksWith $ \CallsBlockWith { calledBlock, argumentsPassed } -> do
            -- TODO assert that the `calledBlock` is one of those in `terminator`
            when (argumentsPassed != []) $ do
                let calledByThisBlock = CalledByBlockWith { callingBlock = blockName, argumentsReceived = argumentsPassed }
                backModifyState (Map.alter (Just . prepend calledByThisBlock . fromMaybe []) calledBlock)
        doModifyM (field @"finishedBlocks") $ \finishedBlocks -> do
            instructions <- getM (field @"unfinishedBlocks" . field @"instructions")
            savedName    <- getM (field @"unfinishedBlocks" . field @"blockName")
            assertM (blockName == savedName)
            let newBlock = BasicBlock savedName instructions (Do terminator)
            return (newBlock : finishedBlocks) -- NOTE: prepending is important for the correctness of `translate`
        modifyM (field @"unfinishedBlocks" ) (assert . previousBlock)

translatedType :: IR.Type IR.Expression -> L.Type
translatedType = \case
    IR.Bool -> i1
    IR.Int  -> i64

allocaForLet :: IR.Name IR.Expression -> Operand
allocaForLet (IR.Name ident nameType _) = LocalReference (ptr (translatedType nameType)) (translatedID ident)

translatedID :: IR.ID node -> L.Name
translatedID = \case
    IR.LetID   num  -> L.mkName (show num)
    IR.BlockID num  -> L.mkName (show num)
    IR.ASTName name -> L.mkName (textToString (Name.renderName name))
    IR.Return       -> "return"

alloca :: LLVM m => IR.Name IR.Expression -> m ()
alloca (IR.Name ident nameType _) = do
    let instr = L.Alloca {
        L.allocatedType = translatedType nameType,
        L.numElements   = Nothing,
        L.alignment     = 0,
        L.metadata      = []
    }
    emit (translatedID ident := instr)

load :: LLVM m => IR.Name IR.Expression -> m Operand
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

store :: LLVM m => IR.Name IR.Expression -> Operand -> m ()
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
    IR.Literal number -> do
        return (numberWithBits 64 (fromIntegral number))
    IR.Named name -> do
        load name

numberWithBits :: Word32 -> Integer -> Operand
numberWithBits bits value = ConstantOperand (LC.Int { LC.integerBits = bits, LC.integerValue = value })

translateUnaryOp :: Operand -> UnaryOperator -> Instruction
translateUnaryOp operand = \case
    Not    -> L.Xor { L.operand0 = numberWithBits 1  1, L.operand1 = operand, L.metadata = [] }
    Negate -> L.Sub { L.operand0 = numberWithBits 64 0, L.operand1 = operand, L.metadata = [], L.nsw = False, L.nuw = False }

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
    IR.Ask text -> do
        printFormat <- translateStringLiteral "%s "
        stringPtr   <- translateStringLiteral text
        emit (Do (call printf [printFormat, stringPtr]))

        scanFormat  <- translateStringLiteral "%26lld\n"
        let instr = L.Alloca { -- TODO factor this out from `alloca`
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
        L.indices  = replicate 2 (numberWithBits 32 0),
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
        emitBlock (translatedID ident) $ do
            translateBlock body
    IR.Let name expr -> do
        alloca name
        operand <- translateExpression expr
        store name operand
    IR.Assign name value -> do
        operand <- translateValue value
        store name operand
    IR.Say text -> do
        formatPtr <- translateStringLiteral "%s\n"
        stringPtr <- translateStringLiteral text
        emit (Do (call printf [formatPtr, stringPtr]))
    IR.Write value -> do
        formatPtr <- translateStringLiteral "%lld\n"
        operand   <- translateValue value
        emit (Do (call printf [formatPtr, operand]))

call :: Operand -> [Operand] -> Instruction
call callee args = L.Call {
    L.tailCallKind       = Nothing,
    L.callingConvention  = LCC.C,
    L.returnAttributes   = [],
    L.function           = Right callee,
    L.arguments          = map (\arg -> (arg, [])) args,
    L.functionAttributes = [],
    L.metadata           = []
}

printf :: Operand
printf = ConstantOperand (GlobalReference (L.FunctionType i32 [ptr i8] True) "printf")

scanf :: Operand
scanf = ConstantOperand (GlobalReference (L.FunctionType i32 [ptr i8] True) "scanf")

translateBlock :: LLVM m => IR.Block -> m (Terminator, [CallsBlockWith])
translateBlock IR.Block { IR.arguments, IR.body, IR.transfer } = do
    translateArguments arguments
    mapM_ translateStatement body
    translateTransfer transfer

translateArguments :: LLVM m => [IR.Name IR.Expression] -> m ()
translateArguments arguments = do
    -- [For each calling block: CalledByBlockWith { callingBlock = block's name, argumentsReceived = [values it passed for arguments] }]
    calledByBlocks <- getArguments

    -- [For each calling block: [For each argument it passed: (the argument's value, the block's name)]]
    argsFromBlocks <- forM calledByBlocks $ \CalledByBlockWith { callingBlock, argumentsReceived } -> do
        assertM (length argumentsReceived == length arguments)
        return (map (\operand -> (operand, callingBlock)) argumentsReceived)

    -- [For each argument: (its IR name, [For each calling block: (the argument's value, the block's name)]]
    let argsWithCallers = zip arguments (transpose argsFromBlocks)
    assertM (length argsWithCallers == length arguments)
    assertM ((length . nub . map (length . snd)) argsWithCallers <= 1) -- double check each block passed the same # of args

    argsWithPhis <- forM argsWithCallers $ \(argument, incomingValues) -> do
        let instr = L.Phi {
            L.type'          = translatedType (IR.nameType argument),
            L.incomingValues = incomingValues,
            L.metadata       = []
        }
        phiName <- freshName
        emit (phiName := instr)
        return (argument, LocalReference (translatedType (IR.nameType argument)) phiName)

    -- We need to do this in two passes because all phis have to be at the beginning of the block (not mixed with e.g. allocas).
    forM_ argsWithPhis $ \(arg, phi) -> do
        -- HACK: We make an alloca for each argument, despite them never being mutated or anything,
        -- just so we can refer to them consistently using the `allocaForLet` mapped names, just like normal `let`s.
        alloca arg
        store arg phi

translateTransfer :: LLVM m => IR.Transfer -> m (Terminator, [CallsBlockWith])
translateTransfer = \case
    IR.Jump target -> do
        callsBlockWith <- translateTarget target
        let instr = L.Br { L.dest = calledBlock callsBlockWith, L.metadata' = [] }
        return (instr, [callsBlockWith])
    IR.Branch value targets -> do
        operand <- translateValue value
        assertM (length targets == 2)
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
    return CallsBlockWith { calledBlock = translatedID (IR.ident targetBlock), argumentsPassed = operands }
