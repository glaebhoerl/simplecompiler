module Name where

import MyPrelude

import qualified Data.Map as Map

import qualified AST
import AST (AST)

-- subscopes within each scope are numbered positionally, starting with 0
type Path = [Int]

data Name info = Name {
    path :: !Path,
    name :: !Text,
    info :: !info
} deriving Show

instance Eq (Name info) where
    n1 == n2 = (path n1, name n1) == (path n2, name n2)

instance Ord (Name info) where
    compare n1 n2 = compare (path n1, name n1) (path n2, name n2)

data Info = Info {
    bindingType :: !AST.BindingType,
    initializer :: !(AST.Expression ResolvedName)
} deriving (Eq, Show)

type ResolvedName = Name Info

data Error
    = NameNotFound    !Text !Path
    | NameWouldShadow !Text !Path
    deriving Show

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
        Just info -> Just (Name (map fst parent) name info)

newtype NameResolve a = NameResolve {
    runNameResolve :: StateT Context (Except Error) a
} deriving (Functor, Applicative, Monad)

instance NameResolveM NameResolve where
    lookupName name = NameResolve $ do
        context <- get
        case findInContext name context of
            Just found -> return found
            Nothing    -> lift (throwE (NameNotFound name (map fst context)))

    inNewScope action = NameResolve $ do
        modify' (prepend (0, Map.empty))
        result <- runNameResolve action
        modify' (assert . tail)
        context <- get
        case context of
            (scopeID, names) : rest -> do
                assertM (scopeID >= 0)
                put ((scopeID + 1, names) : rest)
                return result
            [] -> do
                return result

    bindName name info = NameResolve $ do
        context <- get
        case context of
            [] -> bug "Attempted to bind a name when not in a scope!"
            (scopeID, names) : rest -> do
                when (Map.member name names) $ do
                    lift (throwE (NameWouldShadow name (map fst context)))
                put ((scopeID, Map.insert name info names) : rest)
                return (Name (map fst rest) name info)

resolveNames :: AST Text -> Either Error (AST ResolvedName)
resolveNames ast = runExcept (evalStateT (runNameResolve (resolveNamesIn ast)) [])

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

isWellFormed :: AST ResolvedName -> Bool
isWellFormed = todo

lookupPath :: Path -> AST name -> Maybe (AST name)
lookupPath = todo
