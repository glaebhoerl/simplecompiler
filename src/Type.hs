module Type (Type (..), TypedName, Error (..), checkTypes, typeOf) where

import MyPrelude

import qualified Data.Map as Map

import qualified AST
import AST (AST)
import qualified Name
import Name (Name, NameWith (NameWith), ResolvedName)

-- TODO more info in here
data Error
    = TypeMismatch
    | AssignToLet
    | LiteralOutOfRange
    deriving Show

data Type
    = Int
    | Bool
    deriving (Eq, Show)

-- I think we won't really need the binding type or initializer expression after typechecking anyways?
type TypedName = NameWith Type

class Monad m => TypeCheckM m where
    recordType  :: Name -> Type -> m ()
    lookupType  :: Name -> m (Maybe Type)
    reportError :: Error -> m ()

newtype TypeCheck a = TypeCheck {
    runTypeCheck :: StateT (Map Name Type) (Except Error) a
} deriving (Functor, Applicative, Monad)

instance TypeCheckM TypeCheck where
    recordType name type' = TypeCheck $ do
        liftM (assert . not . Map.member name) get
        modify' (Map.insert name type')
    lookupType name = TypeCheck $ do
        liftM (Map.lookup name) get
    reportError err = TypeCheck $ do
        lift (throwE err)

inferExpression :: TypeCheckM m => AST.Expression ResolvedName -> m Type
inferExpression = \case
    AST.Named name -> do
        getTypeOfName name
    AST.Literal int -> do
        when (int > fromIntegral (maxBound :: Int64)) $ do
            reportError LiteralOutOfRange
        return Int
    AST.UnaryOperator op expr -> do
        let type' = case op of
                Not    -> Bool
                Negate -> Int
        checkExpression type' expr
        return type'
    AST.BinaryOperator expr1 op expr2 -> do
        let (inType, outType) = case op of
                ArithmeticOperator _ -> (Int,  Int)
                ComparisonOperator _ -> (Int,  Bool)
                LogicalOperator    _ -> (Bool, Bool)
        checkExpression inType expr1
        checkExpression inType expr2
        return outType
    AST.Ask _ -> do
        return Int

getTypeOfName :: TypeCheckM m => ResolvedName -> m Type
getTypeOfName (NameWith name info) = do
    haveType <- lookupType name
    case haveType of
        Just type' -> return type'
        Nothing -> do
            type' <- inferExpression (Name.initializer info)
            recordType name type'
            return type'

checkExpression :: TypeCheckM m => Type -> AST.Expression ResolvedName -> m ()
checkExpression expected expr = do
    inferred <- inferExpression expr
    when (expected != inferred) $ do
        reportError TypeMismatch

checkStatement :: TypeCheckM m => AST.Statement ResolvedName -> m ()
checkStatement = \case
    AST.Binding _ name expr -> do
        type' <- inferExpression expr
        recordType (Name.name name) type'
    AST.Assign name expr -> do
        when ((Name.bindingType (Name.info name)) != AST.Var) $ do
            reportError AssignToLet
        nameType <- getTypeOfName name
        checkExpression nameType expr
    AST.IfThen expr block -> do
        checkExpression Bool expr
        mapM_ checkStatement (AST.body block)
    AST.IfThenElse expr block1 block2 -> do
        checkExpression Bool expr
        mapM_ checkStatement (AST.body block1)
        mapM_ checkStatement (AST.body block2)
    AST.Forever block -> do
        mapM_ checkStatement (AST.body block)
    AST.While expr block -> do
        checkExpression Bool expr
        mapM_ checkStatement (AST.body block)
    AST.Return maybeExpr -> do
        mapM_ (checkExpression Int) maybeExpr
    AST.Break -> do
        -- TODO we should check that we're in a loop!!
        return ()
    AST.Say _ -> do
        return ()
    AST.Write expr -> do
        checkExpression Int expr

checkTypes :: AST ResolvedName -> Either Error (AST TypedName)
checkTypes ast = do
    nameToTypeMap <- (runExcept . execStateT Map.empty . runTypeCheck . mapM_ checkStatement . AST.body) ast
    let makeNameTyped (NameWith name _) = NameWith name (assert (Map.lookup name nameToTypeMap))
    return (fmap makeNameTyped ast)

-- this duplicates some information from `inferExpression`, but it doesn't seem worth deduplicating atm
typeOf :: AST.Expression TypedName -> Type
typeOf = \case
    AST.Named name ->
        Name.info name
    AST.Literal _ ->
        Int
    AST.UnaryOperator op _ ->
        case op of
            Not    -> Bool
            Negate -> Int
    AST.BinaryOperator _ op _ ->
        case op of
            ArithmeticOperator _ -> Int
            ComparisonOperator _ -> Bool
            LogicalOperator    _ -> Bool
    AST.Ask _ ->
        Int
