module Type (LargeType (..), Type (..), TypedName, Error (..), TypeMismatch (..), checkTypes, typeOf, ValidationError, validate) where

import MyPrelude

import qualified Data.Map as Map

import qualified Pretty as P
import qualified AST
import AST (AST)
import qualified Name
import Name (Name, NameWith (NameWith), ResolvedName)



------------------------------------------------------------------------ types

data LargeType
    = Type
    | SmallType Type
    deriving (Generic, Eq, Show)

data Type
    = Int
    | Bool
    | Text
    | Unit
    | Function [Type] Type
    deriving (Generic, Eq, Show)

type TypedName = NameWith LargeType

typeOf :: AST.Expression TypedName -> Type
typeOf = \case
    AST.Named name ->
        assert (getWhen (constructor @"SmallType") (Name.info name))
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
    AST.Call name _ ->
        snd (assert (getWhen (constructor @"SmallType" . constructor @"Function") (Name.info name)))



------------------------------------------------------------------------ pretty-printing

instance AST.RenderName TypedName where
    renderName defOrUse (NameWith name type') = (if defOrUse == P.Definition then appendTypeAnnotation else id) (fmap setTypeInNote (AST.renderName defOrUse name))
        where appendTypeAnnotation binding = binding ++ P.colon ++ " " ++ renderedType
              renderedType = P.note (P.Identifier (P.IdentInfo typeName P.Use P.TypeName (Just prettyType))) (P.pretty typeName) where typeName = showText type'
              setTypeInNote = \case
                   P.Identifier info -> P.Identifier (info { P.identType = Just prettyType })
                   _                 -> bug "Pretty-printing annotation on ResolvedName was not Identifier"
              prettyType = case assert (getWhen (constructor @"SmallType") type') of
                  Int          -> P.Int
                  Bool         -> P.Bool
                  Text         -> P.Text
                  Unit         -> todo
                  Function _ _ -> todo



------------------------------------------------------------------------ typechecker frontend

-- TODO more info in here
-- such as the context
-- wonder how to formulate that...
data Error
    = TypeError (TypeMismatch (AST.Expression ResolvedName))
    | CallOfNonFunction -- TODO
    | WrongNumberOfArguments -- TODO
    | UseOfTypeAsExpression -- TODO
    | UseOfExpressionAsType -- TODO
    | AssignToLet
    | LiteralOutOfRange
    deriving (Generic, Show)

data TypeMismatch node = TypeMismatch {
    expectedType :: Type,
    actualType   :: Type,
    expression   :: node
} deriving (Generic, Show)

class Monad m => TypeCheckM m where
    recordType  :: ResolvedName -> Type -> m ()
    lookupType  :: ResolvedName         -> m LargeType
    nameAsType  :: ResolvedName         -> m Type
    reportError :: Error                -> m a

inferExpression :: TypeCheckM m => AST.Expression ResolvedName -> m Type
inferExpression = \case
    AST.Named name -> do
        typeOfName <- lookupType name
        case typeOfName of
            Type                -> reportError UseOfTypeAsExpression
            SmallType smallType -> return smallType
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
    AST.Call function arguments -> do
        functionType <- lookupType function
        case functionType of
            SmallType (Function argumentTypes returnType) -> do
                when (length argumentTypes != length arguments) $ do
                    reportError WrongNumberOfArguments
                zipWithM checkExpression argumentTypes arguments
                return returnType
            SmallType _ -> do
                reportError CallOfNonFunction
            Type -> do
                reportError UseOfTypeAsExpression

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
        inferredType <- inferExpression expr
        recordType name inferredType
    AST.Assign name expr -> do
        when (Name.info name != AST.Var) $ do
            reportError AssignToLet
        nameType <- lookupType name
        case nameType of
            SmallType smallType -> checkExpression smallType expr
            Type                -> reportError UseOfTypeAsExpression
    AST.IfThen expr block -> do
        checkExpression Bool expr
        mapM_ checkStatement (AST.statements block)
    AST.IfThenElse expr block1 block2 -> do
        checkExpression Bool expr
        mapM_ checkStatement (AST.statements block1)
        mapM_ checkStatement (AST.statements block2)
    AST.Forever block -> do
        mapM_ checkStatement (AST.statements block)
    AST.While expr block -> do
        checkExpression Bool expr
        mapM_ checkStatement (AST.statements block)
    AST.Return maybeExpr -> do
        mapM_ (checkExpression Int) maybeExpr -- TODO check against return type!!
    AST.Break -> do
        -- TODO we should check that we're in a loop!!
        return ()
    AST.Expression expr -> do
        unused (inferExpression expr)

checkFunction :: TypeCheckM m => AST.Function ResolvedName -> m ()
checkFunction AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
    argumentTypes <- forM arguments $ \AST.Argument { AST.argumentName, AST.argumentType } -> do
        resolvedType <- nameAsType argumentType
        recordType argumentName resolvedType
        return resolvedType
    returnType <- mapM nameAsType returns
    recordType functionName (Function argumentTypes (fromMaybe Unit returnType))
    mapM_ checkStatement (AST.statements body)



------------------------------------------------------------------------ typechecker backend

newtype TypeCheck a = TypeCheck {
    runTypeCheck :: StateT (Map Name Type) (Except Error) a
} deriving (Functor, Applicative, Monad, MonadState (Map Name Type), MonadError Error)

checkTypes :: AST ResolvedName -> Either Error (AST TypedName)
checkTypes ast = do
    nameToTypeMap <- (runExcept . execStateT Map.empty . runTypeCheck . mapM_ checkFunction) ast
    let makeNameTyped (NameWith name _) = NameWith name (SmallType (assert (Map.lookup name nameToTypeMap)))
    return ((map (fmap makeNameTyped)) ast)

instance TypeCheckM TypeCheck where
    recordType name typeOfName = do
        doModifyState $ \typeMap -> do
            assertM (not (Map.member (Name.name name) typeMap))
            return (Map.insert (Name.name name) typeOfName typeMap)
        return ()
    lookupType = \case
        NameWith (Name.BuiltinName builtin) _ -> do
            return (typeOfBuiltin builtin)
        NameWith otherName                  _ -> do
            liftM (SmallType . assert . Map.lookup otherName) getState
    nameAsType = \case
        NameWith (Name.BuiltinName builtin) _ | Just builtinType <- builtinAsType builtin -> return builtinType
        _ -> reportError UseOfExpressionAsType
    reportError err = do
        throwError err

typeOfBuiltin :: Name.BuiltinName -> LargeType
typeOfBuiltin = \case
    Name.Builtin_Int   -> Type
    Name.Builtin_Bool  -> Type
    Name.Builtin_Text  -> Type
    Name.Builtin_ask   -> SmallType (Function [Text] Int)
    Name.Builtin_say   -> SmallType (Function [Text] Unit)
    Name.Builtin_write -> SmallType (Function [Int]  Unit)

builtinAsType :: Name.BuiltinName -> Maybe Type
builtinAsType = \case
    Name.Builtin_Int   -> Just Int
    Name.Builtin_Bool  -> Just Bool
    Name.Builtin_Text  -> Just Text
    _                  -> Nothing



------------------------------------------------------------------------ validation

type ValidationError = TypeMismatch (AST.Expression TypedName)

-- This checks that:
--  * The AST is locally well-typed at each point, based on the types stored within `Name`s.
-- TODO check type annotations also
-- This does NOT check that:
--  * Names are actually in scope, and the info stored in `Name`s is consistent. Use `Name.validate` for that!
--  * Literals are within range, and assignments match their binding types.
validate :: AST TypedName -> Either ValidationError ()
validate _ = Right () -- TODO!!
{-
validate = runExcept . mapM_ validateFunction where
    validateFunction f = validateBlock (AST.body f) -- TODO
    validateBlock = mapM_ validateStatement . AST.statements
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
        AST.Expression expr -> do
            check todo expr
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
        AST.Call function exprs -> do
            todo function exprs
    check expectedType expr = do
        when (typeOf expr != expectedType) $ do
            throwError (TypeMismatch expectedType (typeOf expr) expr)
        validateExpression expr
-}
