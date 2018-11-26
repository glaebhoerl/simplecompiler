module Type (LargeType (..), Type (..), TypedName, Error (..), TypeMismatch (..), checkTypes, typeOf, ValidationError (..), validate) where

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

typeOf :: AST.Expression TypedName -> Type
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
    actualType   :: Type,
    expression   :: AST.Expression ResolvedName
} deriving (Generic, Show)

class Monad m => TypeCheckM m where
    recordType  :: Type -> ResolvedName -> m ()
    lookupType  :: ResolvedName         -> m Type
    nameAsType  :: ResolvedName         -> m Type
    reportError :: Error                -> m a

inferExpression :: TypeCheckM m => AST.Expression ResolvedName -> m Type
inferExpression = \case
    AST.Named name -> do
        lookupType name
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
            Function argumentTypes returnType -> do
                when (length argumentTypes != length arguments) $ do
                    reportError WrongNumberOfArguments
                zipWithM checkExpression argumentTypes arguments
                return returnType
            _ -> do
                reportError CallOfNonFunction

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
        recordType inferredType name
    AST.Assign name expr -> do
        when (Name.info name != AST.Var) $ do
            reportError AssignToLet
        nameType <- lookupType name
        checkExpression nameType expr
    AST.IfThen expr block -> do
        checkExpression Bool expr
        checkBlock      Unit block
    AST.IfThenElse expr block1 block2 -> do
        checkExpression Bool expr
        checkBlock      Unit block1
        checkBlock      Unit block2
    AST.Forever block -> do
        checkBlock      Unit block
    AST.While expr block -> do
        checkExpression Bool expr
        checkBlock      Unit block
    AST.Return target maybeExpr -> do
        returnType <- lookupType target
        mapM_ (checkExpression returnType) maybeExpr
        when (maybeExpr == Nothing) $ do
            checkExpression Unit (AST.Named target) -- HACK
    AST.Break target -> do
        breakType <- lookupType target
        assertEqM breakType Unit
    AST.Expression expr -> do
        unused (inferExpression expr)

checkBlock :: TypeCheckM m => Type -> AST.Block ResolvedName -> m ()
checkBlock exitTargetType AST.Block { AST.exitTarget, AST.statements } = do
    mapM_ (recordType exitTargetType) exitTarget
    mapM_ checkStatement statements

checkFunction :: TypeCheckM m => AST.Function ResolvedName -> m ()
checkFunction AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
    argumentTypes <- forM arguments $ \AST.Argument { AST.argumentName, AST.argumentType } -> do
        resolvedType <- nameAsType argumentType
        recordType resolvedType argumentName
        return resolvedType
    maybeReturnType <- mapM nameAsType returns
    let returnType = fromMaybe Unit maybeReturnType
    recordType (Function argumentTypes returnType) functionName
    checkBlock returnType body
    when (returnType != Unit && not (definitelyReturns (controlFlow body))) $ do
        reportError FunctionWithoutReturn

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

controlFlow :: Eq name => AST.Block name -> ControlFlow
controlFlow = mconcat . map statementControlFlow . AST.statements where
    statementControlFlow = \case
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
        AST.Forever block ->
            ControlFlow (noBreaks || doesReturn) False where
                -- we can check whether there is a `break` by whether the `exitTarget` is ever referred to
                -- (we make use of the Foldable instances for the AST)
                -- we have to make sure to leave out the `exitTarget` itself!
                noBreaks   = not (any (== (assert (AST.exitTarget block))) (block { AST.exitTarget = Nothing }))
                doesReturn = definitelyReturns (controlFlow block)



------------------------------------------------------------------------ typechecker backend

newtype TypeCheck a = TypeCheck {
    runTypeCheck :: StateT (Map Name Type) (Except Error) a
} deriving (Functor, Applicative, Monad, MonadState (Map Name Type), MonadError Error)

checkTypes :: AST ResolvedName -> Either Error (AST TypedName)
checkTypes ast = do
    nameToTypeMap <- (runExcept . execStateT Map.empty . runTypeCheck . mapM_ checkFunction) ast
    let makeNameTyped (NameWith name _) = NameWith name (assert (oneOf [tryLookup, tryBuiltin])) where
            tryLookup  = fmap SmallType     (Map.lookup name nameToTypeMap)
            tryBuiltin = fmap typeOfBuiltin (match @"BuiltinName" name)
    return (map (fmap makeNameTyped) ast)

instance TypeCheckM TypeCheck where
    recordType typeOfName name = do
        doModifyState $ \typeMap -> do
            assertM (not (Map.member (Name.name name) typeMap))
            return (Map.insert (Name.name name) typeOfName typeMap)
        return ()
    lookupType = \case
        NameWith (Name.BuiltinName builtin) _ -> do
            case typeOfBuiltin builtin of
                SmallType smallType -> return smallType
                Type                -> reportError UseOfTypeAsExpression
        NameWith otherName                  _ -> do
            liftM (assert . Map.lookup otherName) getState
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

-- TODO check presence/absence of exit targets (or in Name.validate??)
data ValidationError
    = ExpectedType LargeType (AST.Expression TypedName)
    | ExpectedFunction (AST.Expression TypedName)
    | ExpectedArgumentCount Int [AST.Expression TypedName]
    deriving (Generic, Show)

-- This checks that:
--  * The AST is locally well-typed at each point, based on the types stored within `Name`s.
--  * The explicitly-written type annotations in the AST are accurate.
-- This does NOT check that:
--  * Names are actually in scope, and the info stored in `Name`s is consistent. Use `Name.validate` for that!
--  * Literals are within range, and assignments match their binding types.
validate :: AST TypedName -> Either ValidationError ()
validate = runExcept . mapM_ validateFunction where
    validateFunction AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
        let getNameAsType = assert . builtinAsType . assert . match @"BuiltinName" . Name.name -- not very nice... :/
        forM_ arguments $ \AST.Argument { AST.argumentName, AST.argumentType } -> do
            validateType argumentType
            check (getNameAsType argumentType) (AST.Named argumentName)
        mapM_ validateType returns
        let functionType = Function argTypes returnType
            argTypes     = map (typeOf . AST.Named . AST.argumentName) arguments
            returnType   = maybe Unit getNameAsType returns
        check functionType (AST.Named functionName)
        mapM_ (check returnType . AST.Named) (AST.exitTarget body) -- TODO check that it's Just!
        validateBlock body
    validateType typeName = do
        when (Name.info typeName != Type) $ do
            throwError (ExpectedType Type (AST.Named typeName))
    validateBlock = mapM_ validateStatement . AST.statements
    validateStatement = \case
        AST.Binding _ (NameWith _ ty) expr -> do
            checkLarge ty expr
        AST.Assign (NameWith _ ty) expr -> do
            checkLarge ty expr
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
        AST.Return target maybeExpr -> do
            mapM_ (check (typeOf (AST.Named target))) maybeExpr
            when (maybeExpr == Nothing) $ do
                check Unit (AST.Named target) -- HACK
        AST.Break target -> do
            check Unit (AST.Named target)
        AST.Expression expr -> do
            validateExpression expr
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
        AST.Call function args -> do
            case typeOf (AST.Named function) of
                Function argTypes _ -> do
                    when (length args != length argTypes) $ do
                        throwError (ExpectedArgumentCount (length argTypes) args)
                    zipWithM_ check argTypes args
                _ -> do
                    throwError (ExpectedFunction (AST.Named function))
    check = checkLarge . SmallType
    checkLarge expectedType expr = do
        when (SmallType (typeOf expr) != expectedType) $ do -- FIXME maybe `validate` shouldn't panic if it sees a `Type` in the wrong place, as `typeOf` does!!
            throwError (ExpectedType expectedType expr)
        validateExpression expr
