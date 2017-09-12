module IR (Type (..), Name (..), Value (..), Expression (..), Statement (..), Block (..), Transfer (..), translate, typeOf) where

import MyPrelude hiding (BinaryOperator (..))

import qualified Data.Map as Map

import qualified MyPrelude as AST (BinaryOperator (..))
import qualified AST       as AST
import qualified Name      as AST
import qualified Type      as AST

data Type node where
    Int        :: Type Expression
    Bool       :: Type Expression
    Parameters :: ![Type Expression] -> Type Block

deriving instance Eq      (Type node)
deriving instance Show    (Type node)

data Name node = Name {
    ident    :: ![Int],
    text     :: !Text,
    nameType :: !(Type node)
} deriving (Generic, Show)

instance Eq (Name node) where
    (==) = (==) `on` ident

instance Ord (Name node) where
    compare = compare `on` ident

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
    = Jump   !(Name Block) ![Value]
    | Branch !Value ![(Name Block, [Value])]
    | Return !Value
    deriving (Generic, Eq, Show)

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

translate :: AST.Block AST.TypedName -> Block
translate = evalState (TranslateState Map.empty []) . runTranslate . (>> todo) .  translateBlock

-- TODO
-- We should insert a `return 0` at the end of programs which "run off the end", I'm not sure where?
-- Is this just the "current continuation" in that case?
-- I think so.

-- begin: emitNewBlock <main block>
--   stash it
--   go forward, emit the CC
--   it is "return 0"
-- every emitNewBlock just stashes the `m ()` argument, but the *parent* `emitNewBlock` goes back and runs them

{-
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

{-
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

data Command = Statement
             | EmitNewBlock [Command]
             | Transfer

class Monad m => TranslateM m where
    translateName       :: AST.Name   -> m (Name Expression)
    emitStatement       :: Statement  -> m ()
    emitLet             :: Expression -> m (Name Expression)
    emitNamedLet        :: AST.Name   -> Expression -> m (Name Expression)
    emitBlock           :: Type Block -> m () -> m (Name Block)
    emitTransfer        :: Transfer   -> m ()
    currentBlock        :: m (Name Block)
    currentArguments    :: m [Name Expression]
    currentContinuation :: Type Block -> m (Name Block)

data TranslateState = TranslateState {
    astNames   :: !(Map AST.Name (Name Expression)),
    blockStack :: ![BlockState]
} deriving Generic

data BlockState = BlockState {
    blockInProgress :: !Block,
    nameCounter     :: !Int
} deriving Generic

newtype Translate a = Translate {
    runTranslate :: State TranslateState a
} deriving (Functor, Applicative, Monad)

getBlockState :: Translate BlockState
getBlockState = Translate $ do
    liftM (assert . head . blockStack) getState

modifyBlockState :: (BlockState -> BlockState) -> Translate ()
modifyBlockState f = Translate $ do
    modifyState $ \state -> case blockStack state of
        [] -> bug "Wanted to modify current block, but there is none!"
        first : rest -> state { blockStack = f first : rest }

newId :: Translate [Int]
newId = do
    modifyBlockState (\blockState -> blockState { nameCounter = nameCounter blockState + 1 })
    blocks <- Translate (liftM blockStack getState)
    return (map nameCounter blocks)

pushBlock :: Type Block -> Translate ()
pushBlock (Parameters argTypes) = do
    joinPoint <- currentContinuation (Parameters [])
    args <- forM argTypes $ \ty -> do
        argId <- newId
        return (Name argId "" ty)
    let newBlock = Block args [] (Jump joinPoint [])
    let newBlockState = BlockState newBlock 0
    Translate $ do
        modifyState (\state -> state { blockStack = prepend newBlockState (blockStack state) })

popBlock :: Translate ()
popBlock = Translate $ do
    modifyState (\state -> state { blockStack = (assert . tail . blockStack) state })

instance TranslateM Translate where
    translateName astName = Translate $ do
        nameMap <- liftM astNames getState
        case Map.lookup astName nameMap of
            Just irName -> do
                return irName
            Nothing -> do
                nameId <- runTranslate newId
                let irName = Name nameId "" todo -- TODO type??
                modifyState (\state -> state { astNames = Map.insert astName irName nameMap })
                return irName

    emitStatement statement = do
        modifyBlockState $ \state ->
            let newStatements = (prepend statement . body . blockInProgress) state in
            let newBlock = (blockInProgress state) { body = newStatements } in
            state { blockInProgress = newBlock  }

    emitLet expr = do
        exprId <- newId
        let name = Name exprId "" (typeOf expr)
        emitStatement (Let name expr)
        return name

    emitNamedLet astName expr = do
        irName <- translateName astName
        todo

    emitBlock paramTypes translateBody = Translate $ do
        todo

    emitTransfer transfer = Translate $ do
        todo

    currentBlock = Translate $ do
        todo

    currentArguments = do
        state <- getBlockState
        return (arguments (blockInProgress state))

    currentContinuation paramTypes = Translate $ do
        todo

translateExpression :: TranslateM m => AST.Expression AST.TypedName -> m Value
translateExpression = \case
    AST.Named (AST.NameWith name _) -> do
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
    AST.BinaryOperator expr1 (AST.ComparisonOperator op) expr2 -> do
        value1 <- translateExpression expr1
        value2 <- translateExpression expr2
        name   <- emitLet (ComparisonOperator value1 op value2)
        return (Named name)
    AST.BinaryOperator expr1 (AST.LogicalOperator op) expr2 -> do
        value1 <- translateExpression expr1
        joinPoint <- currentContinuation (Parameters [Bool])
        rhsBlock <- emitBlock (Parameters []) $ do
            value2 <- translateExpression expr2
            emitTransfer (Jump joinPoint [value2])
        let branches = case op of
                And -> [(joinPoint, [value1]), (rhsBlock,  [])]
                Or  -> [(rhsBlock,  []),       (joinPoint, [value1])]
        emitTransfer (Branch value1 branches)
        args <- currentArguments
        return (Named (assert (head args)))
    AST.Ask text -> do
        name <- emitLet (Ask text)
        return (Named name)

translateStatement :: TranslateM m => AST.Statement AST.TypedName -> m ()
translateStatement = \case
    AST.Binding _ name expr -> do
        value <- translateExpression expr
        _     <- emitNamedLet (AST.name name) (Value value)
        return ()
    AST.Assign name expr -> do
        translatedName <- translateName (AST.name name)
        value <- translateExpression expr
        emitStatement (Assign translatedName value)
    AST.IfThen expr block -> do
        value <- translateExpression expr
        joinPoint <- currentContinuation (Parameters [])
        thenBlock <- emitBlock (Parameters []) $ do
            translateBlock block
        emitTransfer (Branch value [(joinPoint, []), (thenBlock, [])])
    AST.IfThenElse expr block1 block2 -> do
        value <- translateExpression expr
        thenBlock <- emitBlock (Parameters []) $ do
            translateBlock block1
        elseBlock <- emitBlock (Parameters []) $ do
            translateBlock block2
        emitTransfer (Branch value [(elseBlock, []), (thenBlock, [])])
    AST.Forever block -> do
        foreverBlock <- emitBlock (Parameters []) $ do
            self <- currentBlock
            translateBlock block
            emitTransfer (Jump self [])
        emitTransfer (Jump foreverBlock [])
    AST.While expr block -> do
        joinPoint <- currentContinuation (Parameters [])
        whileBlock <- emitBlock (Parameters []) $ do
            self <- currentBlock
            thenBlock <- emitBlock (Parameters []) $ do
                translateBlock block
                emitTransfer (Jump self [])
            value <- translateExpression expr
            emitTransfer (Branch value [(joinPoint, []), (thenBlock, [])])
        emitTransfer (Jump whileBlock [])
    AST.Return maybeExpr -> do
        maybeValue <- mapM translateExpression maybeExpr
        emitTransfer (Return (fromMaybe (Literal 0) maybeValue))
    AST.Break -> do
        todo -- TODO
    AST.Say text -> do
        emitStatement (Say text)
    AST.Write expr -> do
        value <- translateExpression expr
        emitStatement (Write value)

translateBlock :: TranslateM m => AST.Block AST.TypedName -> m ()
translateBlock (AST.Block statements) = mapM_ translateStatement statements
