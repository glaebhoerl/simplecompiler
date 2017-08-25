module IR where

import MyPrelude hiding (BinaryOperator (..))

import qualified MyPrelude as AST (BinaryOperator (..))
import qualified AST       as AST
import qualified Name      as AST
import qualified Type      as AST

data Type node where
    Int        :: Type Expression
    Bool       :: Type Expression
    Parameters :: [Type Expression] -> Type Block

deriving instance Eq   (Type node)
deriving instance Show (Type node)

data Name node = Name {
    ident :: Int,
    text  :: Text,
    type' :: Type node
} deriving Show

instance Eq (Name node) where
    (==) = (==) `on` ident

instance Ord (Name node) where
    compare = compare `on` ident

data Value
    = Literal !Int64
    | Named   !(Name Expression)
    deriving (Eq, Show)

data Expression
    = Value              !Value
    | UnaryOperator      !UnaryOperator             !Value
    | ArithmeticOperator !Value !ArithmeticOperator !Value
    | ComparisonOperator !Value !ComparisonOperator !Value
    | Ask                !Text
    deriving (Eq, Show)

data Statement
    = LetBlock !(Name Block)      !Block
    | Let      !(Name Expression) !Expression
    | Assign   !(Name Expression) !Value
    | Say      !Text
    | Write    !Value
    deriving (Eq, Show)

data Block = Block {
    parameters :: ![Name Expression],
    body       :: ![Statement],
    transfer   :: !Transfer
} deriving (Eq, Show)

data Transfer
    = Jump   !(Name Block) ![Value]
    | Branch !Value ![(Name Block, [Value])]
    | Return !Value
    deriving (Eq, Show)

translate :: AST.Block AST.TypedName -> Block
translate = todo

class Monad m => TranslateM m where
    translateName       :: AST.Name   -> m (Name Expression)
    emitStatement       :: Statement  -> m ()
    emitLet             :: Expression -> m (Name Expression)
    emitNamedLet        :: AST.Name   -> Expression -> m (Name Expression)
    emitBlock           :: Type Block -> m () -> m (Name Block)
    emitTransfer        :: Transfer   -> m ()
    currentBlock        :: m (Name Block)
    currentArguments    :: m [Name Expression]
    currentContinuation :: m (Name Block)
    setCurrentContinuationParameters :: Type Block -> m ()

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
        setCurrentContinuationParameters (Parameters [Bool])
        joinPoint <- currentContinuation
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
        joinPoint <- currentContinuation
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
        joinPoint <- currentContinuation
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
