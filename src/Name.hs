module Name where

import MyPrelude

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Control.Monad.Trans.State.Strict as M
import qualified Control.Monad.Trans.Except       as M

import qualified AST
import AST (AST)

-- subscopes within each scope are numbered positionally, starting with 0
type Path = [Int]

data Name = Name {
    path        :: Path,
    name        :: Text,
    bindingType :: AST.BindingType
} deriving Show

instance Eq Name where
    n1 == n2 = (path n1, name n1) == (path n2, name n2)

instance Ord Name where
    compare n1 n2 = compare (path n1, name n1) (path n1, name n2)

data Error
    = NameNotFound    Text Path
    | NameWouldShadow Text Path
    deriving Show

class Monad m => NameResolveM m where
    lookupName :: Text -> m Name
    inNewScope :: m a  -> m a
    bindName   :: AST.BindingType -> Text -> m Name

-- the stack of scopes we are currently inside
-- fst: how many sub-scopes within that scope we have visited so far
-- snd: the names bound within that scope
-- TODO maybe use Natural and NonEmpty here
type Context = [(Int, Map Text AST.BindingType)]

findInContext :: Text -> Context -> Maybe Name
findInContext name = \case
    [] -> Nothing
    ((_, names) : parent) -> case Map.lookup name names of
        Nothing    -> findInContext name parent
        Just btype -> Just (Name (map fst parent) name btype)

newtype NameResolve a = NameResolve {
    runNameResolve :: M.StateT Context (M.Except Error) a
} deriving (Functor, Applicative, Monad)

instance NameResolveM NameResolve where
    lookupName name = NameResolve $ do
        context <- M.get
        case findInContext name context of
            Just found -> return found
            Nothing    -> lift (M.throwE (NameNotFound name (map fst context)))

    inNewScope action = NameResolve $ do
        M.modify' (prepend (0, Map.empty))
        result <- runNameResolve action
        M.modify' (assert . tail)
        context <- M.get
        case context of
            [] -> return ()
            (scopeID, names) : rest -> do
                assertM (scopeID >= 0)
                M.put ((scopeID + 1, names) : rest)
        return result

    bindName btype name = NameResolve $ do
        context <- M.get
        when (Map.member name (snd (assert (head context)))) $ do
            lift (M.throwE (NameWouldShadow name (map fst context)))
        M.put $ case context of
            []                      -> bug "Attempted to bind a name when not in a scope!"
            (scopeID, names) : rest -> (scopeID, Map.insert name btype names) : rest
        return (Name (map fst (assert (tail context))) name btype)

resolveNames :: AST Text -> Either Error (AST Name)
resolveNames ast = M.runExcept (M.evalStateT (runNameResolve (resolveNamesIn ast)) [])

class ResolveNamesIn node where
    resolveNamesIn :: NameResolveM m => node Text -> m (node Name)

instance ResolveNamesIn AST.Block where
    resolveNamesIn (AST.Block body) = do
        resolvedBody <- inNewScope (mapM resolveNamesIn body)
        return (AST.Block resolvedBody)

instance ResolveNamesIn AST.Statement where
    resolveNamesIn = \case
        AST.Binding btype name expr -> do
            resolvedExpr <- resolveNamesIn expr
            fullName <- bindName btype name
            return (AST.Binding btype fullName resolvedExpr)
        AST.Assign name expr -> do
            resolvedName <- lookupName name
            resolvedExpr <- resolveNamesIn expr
            return (AST.Assign resolvedName resolvedExpr)
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
        AST.Return maybeExpr -> do
            resolvedMaybeExpr <- mapM resolveNamesIn maybeExpr
            return (AST.Return resolvedMaybeExpr)
        AST.Break -> do
            return AST.Break
        AST.Write expr -> do
            resolvedExpr <- resolveNamesIn expr
            return (AST.Write resolvedExpr)

instance ResolveNamesIn AST.Expression where
    resolveNamesIn = \case
        AST.Name name -> do
            resolvedName <- lookupName name
            return (AST.Name resolvedName)
        AST.Literal n -> do
            return (AST.Literal n)
        AST.UnaryOperator op expr -> do
            resolvedExpr <- resolveNamesIn expr
            return (AST.UnaryOperator op resolvedExpr)
        AST.BinaryOperator expr1 op expr2 -> do
            resolvedExpr1 <- resolveNamesIn expr1
            resolvedExpr2 <- resolveNamesIn expr2
            return (AST.BinaryOperator resolvedExpr1 op resolvedExpr2)
        AST.Read -> do
            return AST.Read

isWellFormed :: AST Name -> Bool
isWellFormed = todo

lookupPath :: Path -> AST name -> Maybe (AST name)
lookupPath = todo
