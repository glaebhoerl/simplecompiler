module Name (BuiltinName (..), LocalName (..), Name (..), qualifiedName, unqualifiedName, NameWith (..), Path (..), ResolvedName, Error (..), resolveNames, ValidationError (..), validateNames) where

import MyPrelude

import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as Text

import qualified Pretty as P
import qualified AST
import AST (AST)



------------------------------------------------------------------------ types

data BuiltinName
    = Builtin_Int
    | Builtin_Bool
    | Builtin_Text
    | Builtin_ask
    | Builtin_say
    | Builtin_write
    deriving (Generic, Eq, Ord, Enum, Bounded, Show)

instance Enumerable BuiltinName

-- subscopes within each scope are numbered positionally, starting with 0
data Path = Path {
    function :: Text,
    scope    :: [Int]
} deriving (Generic, Eq, Ord, Show)

data LocalName = LocalName {
    path      :: Path,
    givenName :: Text
} deriving (Generic, Eq, Ord, Show)

data Name
    = BuiltinName  BuiltinName
    | FunctionName Text
    | Name         LocalName
    deriving (Generic, Eq, Ord, Show)

data NameWith info = NameWith {
    name :: Name,
    info :: info
} deriving (Generic, Show, Functor)

instance Eq (NameWith info) where
    (==) = (==) `on` name

instance Ord (NameWith info) where
    compare = compare `on` name

type ResolvedName = NameWith AST.BindingType

qualifiedName :: Name -> Text
qualifiedName = \case
    BuiltinName  name -> unqualifiedName (BuiltinName name)
    FunctionName name -> "." ++ name
    Name    localName -> "." ++ function (path localName) ++ "." ++ Text.intercalate "." (map showText (scope (path localName))) ++ "." ++ givenName localName

unqualifiedName :: Name -> Text
unqualifiedName = \case
    BuiltinName  builtin   -> Text.drop (Text.length "Builtin_" ) (showText builtin)
    FunctionName name      -> name
    Name         localName -> givenName localName



------------------------------------------------------------------------ pretty-printing

instance AST.RenderName Name where
    renderName defOrUse = let makeName isBuiltin nameType name = P.note (P.Identifier (P.IdentInfo name defOrUse nameType isBuiltin)) (P.pretty name) in \case
        Name (LocalName Path { function, scope } given) -> renderedPath ++ renderedGiven
            where pathText      = function ++ "." ++ foldr (\a b -> showText a ++ "." ++ b) "" scope
                  renderedPath  = makeName False P.Block    pathText
                  renderedGiven = makeName False P.Unit     given
        FunctionName name    ->   makeName False P.Function name
        BuiltinName  builtin ->   makeName True  P.Unknown  (unqualifiedName (BuiltinName builtin))

instance AST.RenderName ResolvedName where
    renderName defOrUse (NameWith name _) = AST.renderName defOrUse name



------------------------------------------------------------------------ resolution frontend

class (forall metadata. Monad (m metadata)) => NameResolveM m where
    lookupName     :: Text -> m metadata ResolvedName
    enterScope     :: m metadata a -> m metadata a
    bindName       :: AST.BindingType -> Text -> m metadata ResolvedName
    recordMetadata :: metadata -> m metadata ()

class ResolveNamesIn node where
    resolveNamesIn :: NameResolveM m => node metadata Text -> m metadata (node metadata ResolvedName)

-- TODO properly record metadata for arguments / return type etc.!
instance ResolveNamesIn AST.Function where
    resolveNamesIn AST.Function { AST.functionName, AST.arguments, AST.returns, AST.body } = do
        -- the argument types and return type are in global scope, must be resolved before entering any scope
        argumentTypes <- forM arguments \argument -> do
            (lookupName . AST.argumentType . nodeWithout) argument
        resolvedReturns <- mapM lookupName returns
        -- the argument names are in scope for the body, and may also be shadowed by it
        (resolvedArguments, resolvedBody) <- enterScope do
            resolvedArguments <- forM (zip argumentTypes arguments) \(resolvedType, argument) -> do
                forNodeM argument \AST.Argument { AST.argumentName } -> do
                    resolvedName <- bindName AST.Let argumentName
                    return (AST.Argument resolvedName resolvedType)
            resolvedBody <- resolveNamesIn body
            return (resolvedArguments, resolvedBody)
        return AST.Function {
            AST.functionName = NameWith { name = FunctionName functionName, info = AST.Let }, -- the function itself is bound in `resolveFunction`
            AST.arguments    = resolvedArguments,
            AST.returns      = resolvedReturns,
            AST.body         = resolvedBody
        }

instance ResolveNamesIn AST.Block where
    resolveNamesIn AST.Block { AST.exitTarget, AST.statements } = enterScope do
        resolvedTarget     <- mapM (bindName AST.Let) exitTarget
        resolvedStatements <- mapM resolveNamesIn statements
        return (AST.Block resolvedTarget resolvedStatements)

instance ResolveNamesIn AST.Statement where
    resolveNamesIn = \case
        AST.Binding btype name expr -> do
            -- resolve the expression BEFORE binding the name:
            -- the name should not be in scope for the expression!
            resolvedExpr <- resolveNamesIn expr
            fullName <- bindName btype name
            return (AST.Binding btype fullName resolvedExpr)
        AST.Assign var expr -> do
            resolvedVar  <- lookupName var
            resolvedExpr <- resolveNamesIn expr
            return (AST.Assign resolvedVar resolvedExpr)
        AST.IfThen expr body -> do
            resolvedExpr <- resolveNamesIn expr
            resolvedBody <- resolveNamesIn body
            return (AST.IfThen resolvedExpr resolvedBody)
        AST.IfThenElse expr body1 body2 -> do
            resolvedExpr  <- resolveNamesIn expr
            resolvedBody1 <- resolveNamesIn body1
            resolvedBody2 <- resolveNamesIn body2
            return (AST.IfThenElse resolvedExpr resolvedBody1 resolvedBody2)
        AST.Forever body -> do
            resolvedBody <- resolveNamesIn body
            return (AST.Forever resolvedBody)
        AST.While expr body -> do
            resolvedExpr <- resolveNamesIn expr
            resolvedBody <- resolveNamesIn body
            return (AST.While resolvedExpr resolvedBody)
        AST.Return target maybeExpr -> do
            resolvedTarget <- lookupName target
            resolvedExpr   <- mapM resolveNamesIn maybeExpr
            return (AST.Return resolvedTarget resolvedExpr)
        AST.Break target -> do
            resolvedTarget <- lookupName target
            return (AST.Break resolvedTarget)
        AST.Expression expr -> do
            resolvedExpr <- resolveNamesIn expr
            return (AST.Expression resolvedExpr)

-- We used to be able to do this as just ` mapM lookupName`, but that doesn't record metadata...
-- Wonder if we could do anything to make it work "automatically" again...
instance ResolveNamesIn AST.Expression where
    resolveNamesIn = \case
        AST.Named n -> do
            resolvedName <- lookupName n
            return (AST.Named resolvedName)
        AST.UnaryOperator op expr -> do
            resolvedExpr <- resolveNamesIn expr
            return (AST.UnaryOperator op resolvedExpr)
        AST.BinaryOperator expr1 op expr2 -> do
            resolvedExpr1 <- resolveNamesIn expr1
            resolvedExpr2 <- resolveNamesIn expr2
            return (AST.BinaryOperator resolvedExpr1 op resolvedExpr2)
        AST.Call name exprs -> do
            resolvedName  <- lookupName name
            resolvedExprs <- mapM resolveNamesIn exprs
            return (AST.Call resolvedName resolvedExprs)
        AST.NumberLiteral number -> do
            return (AST.NumberLiteral number)
        AST.TextLiteral text -> do
            return (AST.TextLiteral text)

instance ResolveNamesIn node => ResolveNamesIn (NodeWith node) where
    resolveNamesIn node = do
        recordMetadata (nodeMetadata node)
        mapNodeM resolveNamesIn node



------------------------------------------------------------------------ resolution backend

data Error
    = NameNotFound Text Path
    | NameConflict Text Path
    deriving (Generic, Show)

newtype NameResolve metadata a = NameResolve {
    runNameResolve :: ExceptT Error (State (Context metadata)) a
} deriving (Functor, Applicative, Monad, MonadState (Context metadata), MonadError Error)

resolveNames :: AST metadata Text -> Either (With metadata Error) (AST metadata ResolvedName)
resolveNames = plumbMetadata . runState (Context Set.empty "" [] Nothing) . runExceptT . runNameResolve . mapM resolveFunction where
    plumbMetadata = \case
        (Right result, _      ) -> Right result
        (Left  error,  context) -> Left (With (assert (metadata context)) error)

resolveFunction :: NodeWith AST.Function metadata Text -> NameResolve metadata (NodeWith AST.Function metadata ResolvedName)
resolveFunction = mapNodeM impl where
    impl function@AST.Function { AST.functionName } = do
        doModifyState \Context { functions, locals, metadata } -> do
            assertEqM locals []
            when (Set.member functionName functions) do
                throwError (NameConflict functionName (Path functionName [])) -- TODO should be a nil path instead...?
            return Context { functions = Set.insert functionName functions, currentFunction = functionName, locals, metadata }
        resolveNamesIn function

-- the stack of scopes we are currently inside
-- fst: how many sub-scopes within that scope we have visited so far
-- snd: the names bound within that scope
type LocalContext = [(Int, Map Text AST.BindingType)]  -- TODO maybe use Natural and NonEmpty here

data Context metadata = Context {
    functions       :: Set Text,
    currentFunction :: Text,
    locals          :: LocalContext,
    metadata        :: Maybe metadata
} deriving (Generic, Show)

lookupLocal :: Text -> LocalContext -> Maybe ([Int], AST.BindingType)
lookupLocal name = \case
    [] -> Nothing
    ((_, names) : parent) -> case Map.lookup name names of
        Just bindingType  -> Just (map fst parent, bindingType)
        Nothing           -> lookupLocal name parent

lookupInContext :: Text -> Context metadata -> Maybe ResolvedName
lookupInContext givenName Context { functions, currentFunction, locals } = oneOf [tryLocal, tryFunction, tryBuiltin] where
    tryLocal    = fmap makeLocalName (lookupLocal givenName locals) where
        makeLocalName (scope, info) = NameWith { name = Name LocalName { path = Path { function = currentFunction, scope }, givenName }, info }
    tryFunction = justIf (Set.member givenName functions) NameWith { name = FunctionName givenName, info = AST.Let }
    tryBuiltin  = fmap makeBuiltinName (lookup ("Builtin_" ++ givenName) builtinNames) where
        builtinNames                = map (\builtinName -> (showText builtinName, builtinName)) (enumerate @BuiltinName)
        makeBuiltinName builtinName = NameWith { name = BuiltinName builtinName, info = AST.Let }

instance NameResolveM NameResolve where
    lookupName name = do
        context <- getState
        case lookupInContext name context of
            Just found -> return found
            Nothing    -> throwError (NameNotFound name (Path (currentFunction context) (map fst (locals context))))

    enterScope action = do
        modifyM (field @"locals") (prepend (0, Map.empty))
        result    <- action
        newLocals <- modifyM (field @"locals") (assert . tail)
        case newLocals of
            (scopeID, names) : rest -> do
                assertM (scopeID >= 0)
                setM (field @"locals") ((scopeID + 1, names) : rest)
                return result
            [] -> do
                return result

    bindName info name = do
        context <- getState
        case (locals context) of
            [] -> bug "Attempted to bind a name when not in a scope!"
            (scopeID, names) : rest -> do
                when (Map.member name names) do
                    throwError (NameConflict name (Path (currentFunction context) (map fst (locals context))))
                setM (field @"locals") ((scopeID, Map.insert name info names) : rest)
                return NameWith { name = Name LocalName { path = Path { function = currentFunction context, scope = map fst rest }, givenName = name }, info }

    recordMetadata = setM (field @"metadata") . Just



------------------------------------------------------------------------ validation

data ValidationError info
    = NotInScope   Name
    | Redefined    Name
    | InfoMismatch (NameWith info) (NameWith info)
    deriving (Generic, Show)

-- This checks that:
--  * Each name is in scope where it is used.
--  * A name is not defined more than once by the same scope.
--  * The info stored alongside the name is the same at each of its occurrences.
-- This does NOT check that:
--  * The `path` component of the name is correct. This is regarded as an implementation detail, subject to change.
--  * The binding types are stored correctly. This is an unfortunate limitation of being polymorphic over the `info` type.
validateNames :: Eq info => AST metadata (NameWith info) -> Either (ValidationError info) ()
validateNames = runExcept . evalStateT [Map.empty, builtinNames] . mapM_ validate where
    builtinNames = Map.fromList (zip (map BuiltinName (enumerate @BuiltinName)) (repeat Nothing))

type ValidateM info = StateT [Map Name (Maybe info)] (Except (ValidationError info))

class Validate node where
    validate :: Eq info => node (NameWith info) -> ValidateM info ()

instance Validate (AST.Function metadata) where
    validate function = do
        mapM_ validateName (map (AST.argumentType . nodeWithout) (AST.arguments function))
        mapM_ validateName (AST.returns function)
        recordName (AST.functionName function)
        modifyState (prepend Map.empty)
        mapM_ recordName (map (AST.argumentName . nodeWithout) (AST.arguments function))
        validate (AST.body function)
        modifyState (assert . tail)
        return ()

instance Validate (AST.Block metadata) where
    validate block = do
        modifyState (prepend Map.empty)
        mapM_ recordName        (AST.exitTarget block)
        mapM_ validate (AST.statements block)
        modifyState (assert . tail)
        return ()

instance Validate (AST.Statement metadata) where
    validate = \case
        AST.Binding _ name expr -> do
            validate expr
            recordName name
        AST.Assign n expr -> do
            validate expr
            validateName n
        AST.IfThen expr body -> do
            validate expr
            validate body
        AST.IfThenElse expr body1 body2 -> do
            validate expr
            mapM_ validate [body1, body2]
        AST.Forever body -> do
            validate body
        AST.While expr body -> do
            validate expr
            validate body
        AST.Return target maybeExpr -> do
            validateName target
            mapM_ validate maybeExpr
        AST.Break target -> do
            validateName target
        AST.Expression expr -> do
            validate expr

instance Validate (AST.Expression metadata) where
    validate = \case
        AST.Named n -> do
            validateName n
        AST.UnaryOperator _ expr -> do
            validate expr
        AST.BinaryOperator expr1 _ expr2 -> do
            mapM_ validate [expr1, expr2]
        AST.NumberLiteral _ -> do
            return ()
        AST.TextLiteral _ -> do
            return ()
        AST.Call name exprs -> do
            validateName name
            mapM_ validate exprs

instance Validate (node metadata) => Validate (NodeWith node metadata) where
    validate = validate . nodeWithout

validateName :: Eq info => NameWith info -> ValidateM info ()
validateName (NameWith name info1) = do
    context <- getState
    case Map.lookup name (Map.unions context) of
        Nothing -> do
            throwError (NotInScope name)
        Just Nothing -> do
            return () -- builtin names have no stored info (TODO?)
        Just (Just info2) -> do
            when (info1 != info2) do
                throwError (InfoMismatch (NameWith name info1) (NameWith name info2))

recordName :: Eq info => NameWith info -> ValidateM info ()
recordName (NameWith name info) = do
    doModifyState \context -> do
        let scope = assert (head context)
        when (Map.member name scope) do
            throwError (Redefined name)
        return (prepend (Map.insert name (Just info) scope) (assert (tail context)))
    return ()

