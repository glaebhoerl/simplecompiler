module Type (Type (..), TypedName, Error (..), TypeMismatch (..), checkTypes, typeOf, ValidationError, validate) where

import MyPrelude

import qualified Data.Map as Map

import qualified AST
import AST (AST)
import qualified Name
import Name (Name, NameWith (NameWith), ResolvedName)

-- TODO more info in here
-- such as the context
-- wonder how to formulate that...
data Error
    = TypeError (TypeMismatch (AST.Expression ResolvedName))
    | AssignToLet
    | LiteralOutOfRange
    deriving (Generic, Show)

data TypeMismatch node = TypeMismatch {
    expectedType :: Type,
    actualType   :: Type,
    expression   :: node
} deriving (Generic, Show)

data Type
    = Int
    | Bool
    | Text
    deriving (Generic, Eq, Show)

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
        liftM (assert . not . Map.member name) getState
        modifyState (Map.insert name type')
    lookupType name = TypeCheck $ do
        liftM (Map.lookup name) getState
    reportError err = TypeCheck $ do
        throwError err

inferExpression :: TypeCheckM m => AST.Expression ResolvedName -> m Type
inferExpression = \case
    AST.Named name -> do
        getTypeOfName name
    AST.NumberLiteral int -> do
        when (int > fromIntegral (maxBound :: Int64)) $ do
            reportError LiteralOutOfRange
        return Int
    AST.TextLiteral _ -> do
        return Text
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
    AST.Ask expr -> do
        checkExpression Text expr
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
        reportError $ TypeError TypeMismatch {
            expectedType = expected,
            actualType   = inferred,
            expression   = expr
        }

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
    AST.Say expr -> do
        checkExpression Text expr
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
    AST.NumberLiteral _ ->
        Int
    AST.TextLiteral _ ->
        Text
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

type ValidationError = TypeMismatch (AST.Expression TypedName)

-- This checks that:
--  * The AST is locally well-typed at each point, based on the types stored within `Name`s.
-- This does NOT check that:
--  * Names are actually in scope, and the info stored in `Name`s is consistent. Use `Name.validate` for that!
--  * Literals are within range, and assignments match their binding types.
validate :: AST TypedName -> Either ValidationError ()
validate = runExcept . validateBlock where
    validateBlock = mapM_ validateStatement . AST.body
    validateStatement = \case
        AST.Binding _ (NameWith _ ty) expr -> do
            check ty expr
        AST.Assign (NameWith _ ty) expr -> do
            check ty expr
        AST.IfThen expr body -> do
            check Bool expr
            validateBlock body
        AST.IfThenElse expr body1 body2 -> do
            check Bool expr
            mapM_ validateBlock [body1, body2]
        AST.Forever body -> do
            validateBlock body
        AST.While expr body -> do
            check Bool expr
            validateBlock body
        AST.Return maybeExpr -> do
            mapM_ (check Int) maybeExpr
        AST.Break -> do
            return ()
        AST.Say expr -> do
            check Text expr
        AST.Write expr -> do
            check Int expr
    validateExpression = \case
        AST.UnaryOperator op expr -> do
            check opExpectsType expr where
                opExpectsType = case op of
                    Not    -> Bool
                    Negate -> Int
        AST.BinaryOperator expr1 op expr2 -> do
            mapM_ (check opExpectsType) [expr1, expr2] where
                opExpectsType = case op of
                    ArithmeticOperator _ -> Int
                    ComparisonOperator _ -> Int
                    LogicalOperator    _ -> Bool
        AST.Named _ -> do
            return ()
        AST.NumberLiteral _ -> do
            return ()
        AST.TextLiteral _ -> do
            return ()
        AST.Ask expr -> do
            check Text expr
    check expectedType expr = do
        when (typeOf expr != expectedType) $ do
            throwError (TypeMismatch expectedType (typeOf expr) expr)
        validateExpression expr
