module Name (Name (..), NameWith (..), Path, Info (..), ResolvedName, Error (..), resolveNames, ValidationError (..), validate) where

import MyPrelude

import qualified Data.Map as Map

import qualified Pretty as P
import qualified AST
import AST (AST)

-- subscopes within each scope are numbered positionally, starting with 0
type Path = [Int]

data Name = Name {
    path      :: !Path,
    givenName :: !Text
} deriving (Generic, Eq, Ord, Show)

data NameWith info = NameWith {
    name :: !Name,
    info :: !info
} deriving (Generic, Show, Functor)

instance Eq (NameWith info) where
    (==) = (==) `on` name

instance Ord (NameWith info) where
    compare = compare `on` name

data Info = Info {
    bindingType :: !AST.BindingType,
    initializer :: !(AST.Expression ResolvedName)
} deriving (Generic, Eq, Show)

type ResolvedName = NameWith Info

instance AST.RenderName Name where
    renderName (Name path given) defOrUse = renderedPath ++ renderedGiven
        where pathText      = foldr (\a b -> showText a ++ "." ++ b) "" path
              renderedPath  = P.note (P.Identifier (P.IdentInfo pathText defOrUse P.BlockName Nothing)) (P.pretty pathText)
              renderedGiven = P.note (P.Identifier (P.IdentInfo given    defOrUse P.LetName   Nothing)) (P.pretty given)

instance AST.RenderName ResolvedName where
    renderName (NameWith name _) = AST.renderName name

data Error
    = NameNotFound !Text !Path
    | NameConflict !Text !Path
    deriving (Generic, Show)

class Monad m => NameResolveM m where
    lookupName :: Text -> m ResolvedName
    inNewScope :: m a  -> m a
    bindName   :: Text -> Info -> m ResolvedName

-- the stack of scopes we are currently inside
-- fst: how many sub-scopes within that scope we have visited so far
-- snd: the names bound within that scope
-- TODO maybe use Natural and NonEmpty here
type Context = [(Int, Map Text Info)]

findInContext :: Text -> Context -> Maybe ResolvedName
findInContext name = \case
    [] -> Nothing
    ((_, names) : parent) -> case Map.lookup name names of
        Nothing   -> findInContext name parent
        Just info -> Just (NameWith (Name (map fst parent) name) info)

newtype NameResolve a = NameResolve {
    runNameResolve :: StateT Context (Except Error) a
} deriving (Functor, Applicative, Monad)

instance NameResolveM NameResolve where
    lookupName name = NameResolve $ do
        context <- getState
        case findInContext name context of
            Just found -> return found
            Nothing    -> throwError (NameNotFound name (map fst context))

    inNewScope action = NameResolve $ do
        modifyState (prepend (0, Map.empty))
        result <- runNameResolve action
        modifyState (assert . tail)
        context <- getState
        case context of
            (scopeID, names) : rest -> do
                assertM (scopeID >= 0)
                setState ((scopeID + 1, names) : rest)
                return result
            [] -> do
                return result

    bindName name info = NameResolve $ do
        context <- getState
        case context of
            [] -> bug "Attempted to bind a name when not in a scope!"
            (scopeID, names) : rest -> do
                when (Map.member name names) $ do
                    throwError (NameConflict name (map fst context))
                setState ((scopeID, Map.insert name info names) : rest)
                return (NameWith (Name (map fst rest) name) info)

resolveNames :: AST Text -> Either Error (AST ResolvedName)
resolveNames = runExcept . evalStateT [] . runNameResolve . resolveNamesIn

class ResolveNamesIn node where
    resolveNamesIn :: NameResolveM m => node Text -> m (node ResolvedName)

instance ResolveNamesIn AST.Block where
    resolveNamesIn (AST.Block body) = do
        resolvedBody <- inNewScope (mapM resolveNamesIn body)
        return (AST.Block resolvedBody)

-- all of this is SO CLOSE to just being a `mapM`,
-- except for `bindName` and `inNewScope` for blocks...
-- we could solve `bindName` by having a binding vs. reference sum type
-- but `inNewScope` is flummoxing
instance ResolveNamesIn AST.Statement where
    resolveNamesIn = \case
        AST.Binding btype name expr -> do
            -- resolve the expression BEFORE binding the name:
            -- the name should not be in scope for the expression!
            resolvedExpr <- resolveNamesIn expr
            fullName <- bindName name (Info btype resolvedExpr)
            return (AST.Binding btype fullName resolvedExpr)
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
        ast -> do
            mapM lookupName ast

instance ResolveNamesIn AST.Expression where
    resolveNamesIn = mapM lookupName

data ValidationError info
    = NotInScope   !Name
    | Redefined    !Name
    | InfoMismatch !(NameWith info) !(NameWith info)
    deriving (Generic, Show)

-- This checks that:
--  * Each name is in scope where it is used.
--  * A name is not defined more than once by the same scope.
--  * The info stored alongside the name is the same at each of its occurrences.
-- This does NOT check that:
--  * The `path` component of the name is correct. This is regarded as an implementation detail, subject to change.
--  * The binding types are stored correctly. This is an unfortunate limitation of being polymorphic over the `info` type.
validate :: Eq info => AST (NameWith info) -> Either (ValidationError info) ()
validate = runExcept . evalStateT [] . validateBlock where
    validateBlock block = do
        modifyState (prepend Map.empty)
        mapM_ validateStatement (AST.body block)
        modifyState (assert . tail)
    validateStatement = \case
        AST.Binding _ (NameWith name info) expr -> do
            validateExpression expr
            doModifyState $ \context -> do
                let scope = assert (head context)
                when (Map.member name scope) $ do
                    throwError (Redefined name)
                return (prepend (Map.insert name info scope) (assert (tail context)))
        AST.Assign n expr -> do
            validateExpression expr
            validateName n
        AST.IfThen expr body -> do
            validateExpression expr
            validateBlock body
        AST.IfThenElse expr body1 body2 -> do
            validateExpression expr
            mapM_ validateBlock [body1, body2]
        AST.Forever body -> do
            validateBlock body
        AST.While expr body -> do
            validateExpression expr
            validateBlock body
        AST.Return maybeExpr -> do
            mapM_ validateExpression maybeExpr
        AST.Break -> do
            return ()
        AST.Say expr -> do
            validateExpression expr
        AST.Write expr -> do
            validateExpression expr
    validateExpression = \case
        AST.Named n -> do
            validateName n
        AST.UnaryOperator _ expr -> do
            validateExpression expr
        AST.BinaryOperator expr1 _ expr2 -> do
            mapM_ validateExpression [expr1, expr2]
        AST.NumberLiteral _ -> do
            return ()
        AST.TextLiteral _ -> do
            return ()
        AST.Ask expr -> do
            validateExpression expr
    validateName (NameWith name info1) = do
        context <- getState
        case Map.lookup name (Map.unions context) of
            Nothing -> do
                throwError (NotInScope name)
            Just info2 -> do
                when (info1 != info2) $ do
                    throwError (InfoMismatch (NameWith name info1) (NameWith name info2))
