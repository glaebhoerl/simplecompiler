module Type (LargeType (..), Type (..), TypedName, Error (..), TypeMismatch (..), checkTypes, typeOf, ValidationError (..), validateTypes) where

import MyPrelude

import qualified Data.Map  as Map
import qualified Data.Text as Text

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

typeOf :: AST.Expression metadata TypedName -> Type
typeOf = \case
    AST.Named name -> case Name.info name of
        SmallType smallType ->
            smallType
        Type ->
            bug "Expression of type Type in typed AST"
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
    AST.Call name _ -> case Name.info name of
        SmallType (Function _ returnType) ->
            returnType
        SmallType _ ->
            bug "Call of non-function in typed AST"
        Type ->
            bug "Expression of type Type in typed AST"



------------------------------------------------------------------------ pretty-printing

instance P.Render LargeType where
    render ty = P.note (P.Identifier (P.IdentInfo (typeText ty) P.Use P.Type True)) (P.pretty (typeText ty)) where
        typeText = \case
            SmallType (Function argumentTypes returnType) ->
                "function(" ++ Text.intercalate ", " (map (typeText . SmallType) argumentTypes) ++ ")" ++ case returnType of
                    Unit      -> ""
                    otherType -> " returns " ++ typeText (SmallType otherType)
            SmallType otherType -> showText otherType
            Type                -> "Type"

instance P.Render Type where
    render = P.render . SmallType

instance AST.RenderName TypedName where
    renderName defOrUse (NameWith name ty) = maybeAppendTypeAnnotation (fmap setTypeInNote (AST.renderName defOrUse name))
        where maybeAppendTypeAnnotation binding = binding ++ (if defOrUse == P.Definition then (P.colon ++ " " ++ P.render ty) else "")
              setTypeInNote = \case
                   P.Identifier info -> P.Identifier (info { P.identType })
                   _                 -> bug "Pretty-printing annotation on ResolvedName was not Identifier"
              identType = case ty of
                  Type -> P.Type
                  SmallType smallType -> case smallType of
                      Int          -> P.Int
                      Bool         -> P.Bool
                      Text         -> P.Text
                      Unit         -> P.Unit
                      Function _ _ -> P.Function



------------------------------------------------------------------------ typechecker frontend

-- TODO more info in here
-- such as the context
-- wonder how to formulate that...
data Error
    = TypeError TypeMismatch
    | CallOfNonFunction -- TODO
    | WrongNumberOfArguments -- TODO
    | FunctionWithoutReturn -- TODO
    | UseOfTypeAsExpression -- TODO
    | UseOfExpressionAsType -- TODO
    | AssignToLet
    | LiteralOutOfRange
    deriving (Generic, Show)

data TypeMismatch = TypeMismatch {
    expectedType :: Type,
    actualType   :: Type
    --expression   :: AST.Expression ResolvedName -- I think instead of this, just the location is enough?
} deriving (Generic, Show)

class (forall metadata. Monad (m metadata)) => TypeCheckM m where
    recordType     :: Type -> ResolvedName -> m metadata ()
    lookupType     :: ResolvedName         -> m metadata Type
    nameAsType     :: ResolvedName         -> m metadata Type
    reportError    :: Error                -> m metadata a
    recordMetadata :: metadata             -> m metadata ()

class CheckTypeOf node where
    inferType :: TypeCheckM m => node metadata ResolvedName -> m metadata Type
    checkUnit :: TypeCheckM m => node metadata ResolvedName -> m metadata ()
    inferType node = do
        checkUnit node
        return Unit
    checkUnit = checkType Unit

checkType :: (TypeCheckM m, CheckTypeOf node) => Type -> node metadata ResolvedName -> m metadata ()
checkType expectedType node = do
    actualType <- inferType node
    when (expectedType != actualType) do
        reportError (TypeError TypeMismatch { expectedType, actualType })

instance CheckTypeOf AST.Expression where
    inferType = \case
        AST.Named name -> do
            lookupType name
        AST.NumberLiteral int -> do
            when (int > fromIntegral (maxBound :: Int64)) do
                reportError LiteralOutOfRange
            return Int
        AST.TextLiteral _ -> do
            return Text
        AST.UnaryOperator op expr -> do
            let type' = case op of
                    Not    -> Bool
                    Negate -> Int
            checkType type' expr
            return type'
        AST.BinaryOperator expr1 op expr2 -> do
            let (inType, outType) = case op of
                    ArithmeticOperator _ -> (Int,  Int)
                    ComparisonOperator _ -> (Int,  Bool)
                    LogicalOperator    _ -> (Bool, Bool)
            checkType inType expr1
            checkType inType expr2
            return outType
        AST.Call function arguments -> do
            functionType <- lookupType function
            case functionType of
                Function argumentTypes returnType -> do
                    when (length argumentTypes != length arguments) do
                        reportError WrongNumberOfArguments
                    zipWithM checkType argumentTypes arguments
                    return returnType
                _ -> do
                    reportError CallOfNonFunction

instance CheckTypeOf AST.Statement where
    checkUnit = \case
        AST.Binding _ name expr -> do
            inferredType <- inferType expr
            recordType inferredType name
        AST.Assign name expr -> do
            when (Name.info name != AST.Var) do
                reportError AssignToLet
            nameType <- lookupType name
            checkType nameType expr
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
            mapM_ (checkType returnType) maybeExpr
            when (maybeExpr == Nothing) do
                checkType Unit (AST.Named target) -- HACK
        AST.Break target -> do
            breakType <- lookupType target
            assertEqM breakType Unit
        AST.Expression expr -> do
            unused (inferType expr)

instance CheckTypeOf AST.Block where
    checkUnit AST.Block { AST.statements } = do
        mapM_ checkUnit statements

checkBlock :: TypeCheckM m => Type -> NodeWith AST.Block metadata ResolvedName -> m metadata ()
checkBlock exitTargetType block = do
    recordType exitTargetType (assert (AST.exitTarget (nodeWithout block)))
    checkUnit block

instance CheckTypeOf AST.Function where
    checkUnit AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
        argumentTypes <- forM arguments \argument -> do
            let AST.Argument { AST.argumentName, AST.argumentType } = nodeWithout argument
            resolvedType <- nameAsType argumentType
            recordType resolvedType argumentName
            return resolvedType
        maybeReturnType <- mapM nameAsType returns
        let returnType = fromMaybe Unit maybeReturnType
        recordType (Function argumentTypes returnType) functionName
        checkBlock returnType body
        when (returnType != Unit && not (definitelyReturns (controlFlow body))) do
            reportError FunctionWithoutReturn

instance CheckTypeOf node => CheckTypeOf (NodeWith node) where
    inferType node = do
        recordMetadata (nodeMetadata node)
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
    types    :: Map Name Type,
    metadata :: Maybe metadata
} deriving (Generic, Show)

checkTypes :: AST metadata ResolvedName -> Either (With metadata Error) (AST metadata TypedName)
checkTypes ast = pipeline ast where
    pipeline        = constructResult . runState initialState . runExceptT . runTypeCheck . mapM_ checkUnit
    initialState    = TypeCheckState Map.empty Nothing
    constructResult = \case
        (Right (),    TypeCheckState { types    }) -> Right (map (fmap (makeNameTyped types)) ast)
        (Left  error, TypeCheckState { metadata }) -> Left  (With (assert metadata) error)
    makeNameTyped types (NameWith name _) =
        NameWith name (assert (oneOf [tryLookup, tryBuiltin])) where
            tryLookup  = fmap SmallType     (Map.lookup name types)
            tryBuiltin = fmap typeOfBuiltin (match @"BuiltinName" name)

instance TypeCheckM TypeCheck where
    recordType typeOfName name = do
        doModifyM (field @"types") \typeMap -> do
            assertM (not (Map.member (Name.name name) typeMap))
            return (Map.insert (Name.name name) typeOfName typeMap)
        return ()
    lookupType = \case
        NameWith (Name.BuiltinName builtin) _ -> do
            case typeOfBuiltin builtin of
                SmallType smallType -> return smallType
                Type                -> reportError UseOfTypeAsExpression
        NameWith otherName                  _ -> do
            liftM (assert . Map.lookup otherName) (getM (field @"types"))
    nameAsType = \case
        NameWith (Name.BuiltinName builtin) _ | Just builtinType <- builtinAsType builtin -> return builtinType
        _ -> reportError UseOfExpressionAsType
    reportError err = do
        throwError err
    recordMetadata = do
        setM (field @"metadata") . Just

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

-- TODO check presence/absence of exit targets (or in Name.validate??)
data ValidationError metadata
    = ExpectedType LargeType (AST.Expression metadata TypedName)
    | ExpectedFunction (AST.Expression metadata TypedName)
    | ExpectedArgumentCount Int [NodeWith AST.Expression metadata TypedName]
    deriving (Generic, Show)

type ValidateM a = Except (ValidationError a)

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

instance Validate AST.Function where
    validate AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
        let getNameAsType = assert . builtinAsType . assert . match @"BuiltinName" . Name.name -- not very nice... :/
            validateType typeName = do
                when (Name.info typeName != Type) do
                    throwError (ExpectedType Type (AST.Named typeName))
        forM_ arguments \argument -> do
            let AST.Argument { AST.argumentName, AST.argumentType } = nodeWithout argument
            validateType argumentType
            checkName (getNameAsType argumentType) argumentName
        mapM_ validateType returns
        let functionType = Function argTypes returnType
            argTypes     = map (typeOf . AST.Named . AST.argumentName . nodeWithout) arguments
            returnType   = maybe Unit getNameAsType returns
        checkName functionType functionName
        mapM_ (checkName returnType) ((AST.exitTarget . nodeWithout) body) -- TODO check that it's Just!
        validate body

instance Validate AST.Block where
    validate = mapM_ validate . AST.statements

instance Validate AST.Statement where
    validate = \case
        AST.Binding _ (NameWith _ ty) expr -> do
            checkLarge ty expr
        AST.Assign (NameWith _ ty) expr -> do
            checkLarge ty expr
        AST.IfThen expr body -> do
            check Bool expr
            validate body
        AST.IfThenElse expr body1 body2 -> do
            check Bool expr
            mapM_ validate [body1, body2]
        AST.Forever body -> do
            validate body
        AST.While expr body -> do
            check Bool expr
            validate body
        AST.Return target maybeExpr -> do
            mapM_ (check (typeOf (AST.Named target))) maybeExpr
            when (maybeExpr == Nothing) do
                checkName Unit target
        AST.Break target -> do
            checkName Unit target
        AST.Expression expr -> do
            validate expr

instance Validate AST.Expression where
    validate = \case
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
        AST.Call function args -> do
            case typeOf (AST.Named function) of
                Function argTypes _ -> do
                    when (length args != length argTypes) do
                        throwError (ExpectedArgumentCount (length argTypes) args)
                    zipWithM_ check argTypes args
                _ -> do
                    throwError (ExpectedFunction (AST.Named function))

instance Validate node => Validate (NodeWith node) where
    validate = validate . nodeWithout

checkName :: Type -> TypedName -> ValidateM metadata ()
checkName ty name = checkImpl (SmallType ty) (AST.Named name)

check :: Type -> NodeWith AST.Expression metadata TypedName -> ValidateM metadata ()
check = checkLarge . SmallType

checkLarge :: LargeType -> NodeWith AST.Expression metadata TypedName -> ValidateM metadata ()
checkLarge expectedType = checkImpl expectedType . nodeWithout

checkImpl :: LargeType -> AST.Expression metadata TypedName -> ValidateM metadata ()
checkImpl expectedType expr = do
    when (SmallType (typeOf expr) != expectedType) do -- FIXME maybe `validate` shouldn't panic if it sees a `Type` in the wrong place, as `typeOf` does!!
        throwError (ExpectedType expectedType expr)
    validate expr
