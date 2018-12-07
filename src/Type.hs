module Type (TypeInfo (..), Type (..), TypedName, Error (..), checkTypes, typeOf, ValidationError (..), validateTypes) where

import MyPrelude

import qualified Data.Map  as Map
import qualified Data.Text as Text

import qualified Pretty as P
import qualified AST
import AST (AST)
import qualified Name
import Name (Name, NameWith (NameWith), ResolvedName)



------------------------------------------------------------------------ types

data TypeInfo
    = IsType  Type
    | HasType Type
    deriving (Generic, Eq, Show)

data Type
    = Int
    | Bool
    | Text
    | Unit
    | Function [Type] Type
    deriving (Generic, Eq, Show)

type TypedName = NameWith TypeInfo

typeOf :: AST.Expression metadata TypedName -> Type
typeOf = \case
    AST.Named name -> case Name.info name of
        HasType ty ->
            ty
        IsType _ ->
            bug "Expression which IsType in typed AST"
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
    AST.Call fn _ -> case typeOf (nodeWithout fn) of
        Function _ returnType ->
            returnType
        _ ->
            bug "Call of non-function in typed AST"



------------------------------------------------------------------------ pretty-printing

instance P.Render TypeInfo where
    render ty = P.note (P.Identifier (P.IdentInfo (typeText ty) P.Use P.Type True)) (P.pretty (typeText ty)) where
        typeText = \case
            HasType (Function argumentTypes returnType) ->
                "function(" ++ Text.intercalate ", " (map (typeText . HasType) argumentTypes) ++ ")" ++ case returnType of
                    Unit      -> ""
                    otherType -> " returns " ++ typeText (HasType otherType)
            HasType otherType -> showText otherType
            IsType  _         -> "Type"

instance P.Render Type where
    render = P.render . HasType

instance AST.RenderName TypedName where
    renderName defOrUse (NameWith name ty) = maybeAppendTypeAnnotation (fmap setTypeInNote (AST.renderName defOrUse name))
        where maybeAppendTypeAnnotation binding = binding ++ (if defOrUse == P.Definition then (P.colon ++ " " ++ P.render ty) else "")
              setTypeInNote = \case
                   P.Identifier info -> P.Identifier (info { P.identType })
                   _                 -> bug "Pretty-printing annotation on ResolvedName was not Identifier"
              identType = case ty of
                  IsType _ -> P.Type
                  HasType hasType -> case hasType of
                      Int          -> P.Int
                      Bool         -> P.Bool
                      Text         -> P.Text
                      Unit         -> P.Unit
                      Function _ _ -> P.Function



------------------------------------------------------------------------ typechecker frontend

-- TODO more info in here?
data Error
    = TypeMismatch Expected TypeInfo
    | WrongNumberOfArguments
    | FunctionWithoutReturn
    | AssignToLet
    | LiteralOutOfRange
    deriving (Generic, Show)

data Expected
    = Expected Type
    | ExpectedFunction
    | ExpectedExpression
    | ExpectedType
    deriving (Generic, Show)

class (forall metadata. Monad (m metadata)) => TypeCheckM m where
    recordType    :: Type -> ResolvedName     -> m metadata () -- for now, the only things which are `IsType` are builtins
    lookupType    :: ResolvedName             -> m metadata TypeInfo
    reportError   :: Error                    -> m metadata a
    enterMetadata :: metadata -> m metadata a -> m metadata a

-- TODO deduplicate?
enterMetadataOf :: TypeCheckM m => NodeWith node metadata name -> m metadata a -> m metadata a
enterMetadataOf = enterMetadata . nodeMetadata

class CheckTypeOf node where
    inferType :: TypeCheckM m => node metadata ResolvedName -> m metadata TypeInfo
    checkUnit :: TypeCheckM m => node metadata ResolvedName -> m metadata ()
    inferType node = do
        checkUnit node
        return (HasType Unit)
    checkUnit = checkType Unit

checkType :: (TypeCheckM m, CheckTypeOf node) => Type -> node metadata ResolvedName -> m metadata ()
checkType expected node = do
    actual <- inferType node
    when (actual != HasType expected) do
        reportError (TypeMismatch (Expected expected) actual)

instance CheckTypeOf AST.Expression where
    inferType = \case
        AST.Named name -> do
            nameType <- lookupType name
            case nameType of
                HasType _ -> return nameType
                IsType  _ -> reportError (TypeMismatch ExpectedExpression nameType)
        AST.NumberLiteral int -> do
            when (int > fromIntegral (maxBound :: Int64) || int < fromIntegral (minBound :: Int64)) do
                reportError LiteralOutOfRange
            return (HasType Int)
        AST.TextLiteral _ -> do
            return (HasType Text)
        AST.UnaryOperator op expr -> do
            let type' = case op of
                    Not    -> Bool
                    Negate -> Int
            checkType type' expr
            return (HasType type')
        AST.BinaryOperator expr1 op expr2 -> do
            let (inType, outType) = case op of
                    ArithmeticOperator _ -> (Int,  Int)
                    ComparisonOperator _ -> (Int,  Bool)
                    LogicalOperator    _ -> (Bool, Bool)
            checkType inType expr1
            checkType inType expr2
            return (HasType outType)
        AST.Call function arguments -> do
            functionType <- inferType function
            case functionType of
                HasType (Function argumentTypes returnType) -> do
                    when (length argumentTypes != length arguments) do
                        reportError WrongNumberOfArguments
                    zipWithM checkType argumentTypes arguments
                    return (HasType returnType)
                _ -> do
                    reportError (TypeMismatch ExpectedFunction functionType)

instance CheckTypeOf AST.Statement where
    checkUnit = \case
        AST.Binding _ name expr -> do
            inferred <- inferType expr
            case inferred of
                HasType hasType -> recordType hasType name
                IsType  _       -> reportError (TypeMismatch ExpectedExpression inferred)
        AST.Assign name expr -> do
            when (Name.info name != AST.Var) do
                reportError AssignToLet
            nameType <- lookupType name
            case nameType of
                HasType hasType -> checkType hasType expr
                IsType  _       -> reportError (TypeMismatch ExpectedExpression nameType)
        AST.IfThen expr block -> do
            checkType Bool expr
            checkUnit block
        AST.IfThenElse expr block1 block2 -> do
            checkType Bool expr
            checkUnit block1
            checkUnit block2
        AST.Forever block -> do
            checkBlock Unit block
        AST.While expr block -> do
            checkType Bool expr
            checkBlock Unit block
        AST.Return target maybeExpr -> do
            returnType <- lookupType target
            mapM_ (checkType (assert (match @"HasType" returnType))) maybeExpr
            when (maybeExpr == Nothing) do
                checkType Unit (AST.Named target) -- HACK?
        AST.Break target -> do
            breakType <- lookupType target
            assertEqM breakType (HasType Unit)
        AST.Expression expr -> do
            unused (inferType expr)

instance CheckTypeOf AST.Block where
    checkUnit AST.Block { AST.statements } = do
        mapM_ checkUnit statements

checkBlock :: TypeCheckM m => Type -> NodeWith AST.Block metadata ResolvedName -> m metadata ()
checkBlock exitTargetType block = do
    recordType exitTargetType (assert (AST.exitTarget (nodeWithout block)))
    checkUnit block

resolveAsType :: TypeCheckM m => NodeWith AST.Type metadata ResolvedName -> m metadata Type
resolveAsType typeNode = enterMetadataOf typeNode do
    resolved <- inferType (nodeWithout typeNode)
    return (msgAssert "type annotation resolved to a non-type" (match @"IsType" resolved))

instance CheckTypeOf AST.Function where
    checkUnit AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
        argumentTypes <- forM arguments \argument -> do
            enterMetadataOf argument do
                let AST.Argument { AST.argumentName, AST.argumentType } = nodeWithout argument
                resolvedType <- resolveAsType argumentType
                recordType resolvedType argumentName
                return resolvedType
        maybeReturnType <- mapM resolveAsType returns
        let returnType = fromMaybe Unit maybeReturnType
        recordType (Function argumentTypes returnType) functionName
        checkBlock returnType body
        when (returnType != Unit && not (definitelyReturns (controlFlow body))) do
            reportError FunctionWithoutReturn

instance CheckTypeOf AST.Type where
    inferType = \case
        AST.NamedType name -> do
            nameType <- lookupType name
            case nameType of
                IsType  _ -> return nameType
                HasType _ -> reportError (TypeMismatch ExpectedType nameType)
        AST.FunctionType parameters returns -> do
            resolvedParameters <- mapM resolveAsType parameters
            resolvedReturns    <- resolveAsType returns
            return (IsType (Function resolvedParameters resolvedReturns))

instance CheckTypeOf node => CheckTypeOf (NodeWith node) where
    inferType node = do
        enterMetadataOf node do
            inferType (nodeWithout node)

data ControlFlow = ControlFlow {
    definitelyReturns :: Bool, -- guaranteed divergence also counts as "returning"
    potentiallyBreaks :: Bool
}

instance Semigroup ControlFlow where
    prev <> next = ControlFlow returns breaks where
        returns = definitelyReturns prev || (not (potentiallyBreaks prev) && definitelyReturns next)
        breaks  = potentiallyBreaks prev || (not (definitelyReturns prev) && potentiallyBreaks next)

instance Monoid ControlFlow where
    mempty = ControlFlow False False

class CheckControlFlow node where
    controlFlow :: Eq name => node metadata name -> ControlFlow

instance CheckControlFlow AST.Block where
    controlFlow = mconcat . map controlFlow . AST.statements

instance CheckControlFlow AST.Statement where
    controlFlow = \case
        AST.Return {} ->
            ControlFlow True  False
        AST.Break {} ->
            ControlFlow False True
        AST.Binding {} ->
            ControlFlow False False
        AST.Assign {} ->
            ControlFlow False False
        AST.Expression {} ->
            ControlFlow False False
        AST.While {} ->
            ControlFlow False False -- loops can't currently break out of the /outer/ context
        AST.IfThen _ block ->
            ControlFlow False (potentiallyBreaks (controlFlow block))
        AST.IfThenElse _ block1 block2 ->
            ControlFlow (returns1 && returns2) (breaks1 || breaks2) where
                ControlFlow returns1 breaks1 = controlFlow block1
                ControlFlow returns2 breaks2 = controlFlow block2
        AST.Forever blockWith ->
            ControlFlow (noBreaks || doesReturn) False where
                -- we can check whether there is a `break` by whether the `exitTarget` is ever referred to
                -- (we make use of the Foldable instances for the AST)
                -- we have to make sure to leave out the `exitTarget` itself!
                noBreaks   = not (any (== (assert (AST.exitTarget block))) (block { AST.exitTarget = Nothing }))
                doesReturn = definitelyReturns (controlFlow block)
                block      = nodeWithout blockWith

instance CheckControlFlow node => CheckControlFlow (NodeWith node) where
    controlFlow = controlFlow . nodeWithout


------------------------------------------------------------------------ typechecker backend

newtype TypeCheck metadata a = TypeCheck {
    runTypeCheck :: (ExceptT Error) (State (TypeCheckState metadata)) a
} deriving (Functor, Applicative, Monad, MonadState (TypeCheckState metadata), MonadError Error)

data TypeCheckState metadata = TypeCheckState {
    types    :: Map Name TypeInfo,
    metadata :: [metadata]
} deriving (Generic, Show)

checkTypes :: AST metadata ResolvedName -> Either (With metadata Error) (AST metadata TypedName)
checkTypes ast = pipeline ast where
    pipeline        = constructResult . runState initialState . runExceptT . runTypeCheck . mapM_ checkUnit
    initialState    = TypeCheckState (Map.fromList builtinNames) []
    builtinNames    = map (\builtinName -> (Name.BuiltinName builtinName, inferBuiltin builtinName)) (enumerate @Name.BuiltinName)
    constructResult = \case
        (Right (),    TypeCheckState { types    }) -> Right (map (fmap (makeNameTyped types)) ast)
        (Left  error, TypeCheckState { metadata }) -> Left  (With (assert (head metadata)) error)
    makeNameTyped types (NameWith name _) = NameWith name (assert (Map.lookup name types))

instance TypeCheckM TypeCheck where
    recordType typeOfName name = do
        doModifyM (field @"types") \typeMap -> do
            assertM (not (Map.member (Name.name name) typeMap))
            return (Map.insert (Name.name name) (HasType typeOfName) typeMap)
        return ()
    lookupType name = do
        typeMap <- getM (field @"types")
        return (msgAssert "Name not found in types map!" (Map.lookup (Name.name name) typeMap))
    reportError err = do
        throwError err
    enterMetadata metadata action = do -- TODO maybe deduplicate this from here and `Name`?
        modifyM (field @"metadata") (prepend metadata)
        result <- action
        modifyM (field @"metadata") (assert . tail)
        return result

inferBuiltin :: Name.BuiltinName -> TypeInfo
inferBuiltin = \case
    Name.Builtin_Int   -> IsType Int
    Name.Builtin_Bool  -> IsType Bool
    Name.Builtin_Text  -> IsType Text
    Name.Builtin_Unit  -> IsType Unit
    Name.Builtin_ask   -> HasType (Function [Text] Int)
    Name.Builtin_say   -> HasType (Function [Text] Unit)
    Name.Builtin_write -> HasType (Function [Int]  Unit)



------------------------------------------------------------------------ validation

-- TODO check presence/absence of exit targets (or in Name.validate??)
data ValidationError metadata
    = BadType          Expected (AST.Expression metadata TypedName)
    | BadAnnotation    Type     (AST.Type       metadata TypedName)
    | BadArgumentCount Int      (AST.Expression metadata TypedName)
    deriving (Generic, Show)

type ValidateM metadata a = Except (ValidationError metadata) a

class Validate node where
    validate :: node metadata TypedName -> ValidateM metadata ()

-- This checks that:
--  * The AST is locally well-typed at each point, based on the types stored within `Name`s.
--  * The explicitly-written type annotations in the AST are accurate.
-- This does NOT check that:
--  * Names are actually in scope, and the info stored in `Name`s is consistent. Use `Name.validate` for that!
--  * Literals are within range, and assignments match their binding types.
validateTypes :: AST metadata TypedName -> Either (ValidationError metadata) ()
validateTypes = runExcept . mapM_ validate

validateAnnotation :: Type -> AST.Type metadata TypedName -> ValidateM metadata ()
validateAnnotation = curry `strictly` \case -- FIXME
    (expected@(Function expectedParams expectedReturns), annotated@(AST.FunctionType annotatedParams annotatedReturns)) -> do
        when (length expectedParams != length annotatedParams) do
            throwError (BadAnnotation expected annotated)
        zipWithM_ validateAnnotation expectedParams (map nodeWithout annotatedParams)
        validateAnnotation expectedReturns (nodeWithout annotatedReturns)
    (expectedType, AST.NamedType namedType) | Name.info namedType == IsType expectedType -> do
        return ()
    (expectedType, annotation) -> do
        throwError (BadAnnotation expectedType annotation)

instance Validate AST.Function where
    validate AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
        argumentTypes <- forM arguments \argument -> do
            let AST.Argument { AST.argumentName, AST.argumentType } = nodeWithout argument
            case Name.info argumentName of
                HasType ty -> do
                    validateAnnotation ty (nodeWithout argumentType)
                    return ty
                IsType _ -> do
                    throwError (BadType ExpectedExpression (AST.Named argumentName))
        case Name.info functionName of
            HasType infoType@(Function infoParams infoReturns) -> do
                mapM_ (validateAnnotation infoReturns . nodeWithout) returns
                when (infoParams != argumentTypes) do
                    throwError (BadType (Expected infoType) (AST.Named functionName)) -- ehhhhhh
                mapM_ (validateName infoReturns) ((AST.exitTarget . nodeWithout) body) -- TODO check that it's Just!
            HasType _ -> do
                throwError (BadType ExpectedFunction (AST.Named functionName))
            IsType _ -> do
                throwError (BadType ExpectedExpression (AST.Named functionName))
        validate body

instance Validate AST.Block where
    validate = mapM_ validate . AST.statements

instance Validate AST.Statement where
    validate = \case
        AST.Binding _ name@(NameWith _ info) expr -> do
            case info of
                HasType ty -> validateExpr ty expr
                IsType  _  -> throwError (BadType ExpectedExpression (AST.Named name))
        AST.Assign name@(NameWith _ info) expr -> do
            case info of
                HasType ty -> validateExpr ty expr
                IsType  _  -> throwError (BadType ExpectedExpression (AST.Named name))
        AST.IfThen expr body -> do
            validateExpr Bool expr
            validate body
        AST.IfThenElse expr body1 body2 -> do
            validateExpr Bool expr
            mapM_ validate [body1, body2]
        AST.Forever body -> do
            validate body
        AST.While expr body -> do
            validateExpr Bool expr
            validate body
        AST.Return target maybeExpr -> do
            mapM_ (validateExpr (typeOf (AST.Named target))) maybeExpr
            when (maybeExpr == Nothing) do
                validateName Unit target
        AST.Break target -> do
            validateName Unit target
        AST.Expression expr -> do
            validate expr

instance Validate AST.Expression where
    validate = \case
        AST.UnaryOperator op expr -> do
            validateExpr opExpectsType expr where
                opExpectsType = case op of
                    Not    -> Bool
                    Negate -> Int
        AST.BinaryOperator expr1 op expr2 -> do
            mapM_ (validateExpr opExpectsType) [expr1, expr2] where
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
        AST.Call function args -> do
            case typeOf (nodeWithout function) of
                Function argTypes _ -> do
                    when (length args != length argTypes) do
                        throwError (BadArgumentCount (length argTypes) (AST.Call function args))
                    zipWithM_ validateExpr argTypes args
                _ -> do
                    throwError (BadType ExpectedFunction (nodeWithout function))

instance Validate node => Validate (NodeWith node) where
    validate = validate . nodeWithout

validateName :: Type -> TypedName -> ValidateM metadata ()
validateName ty = validateExprImpl ty . AST.Named

validateExpr :: Type -> NodeWith AST.Expression metadata TypedName -> ValidateM metadata ()
validateExpr ty = validateExprImpl ty . nodeWithout

validateExprImpl :: Type -> AST.Expression metadata TypedName -> ValidateM metadata ()
validateExprImpl expectedType expr = do
    when (typeOf expr != expectedType) do -- FIXME maybe `validate` shouldn't panic if it sees a `Type` in the wrong place, as `typeOf` does!!
        throwError (BadType (Expected expectedType) expr)
    validate expr
