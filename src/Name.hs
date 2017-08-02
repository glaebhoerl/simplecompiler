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
        assertM (fst (assert (head context)) >= 0)
        M.put $ case context of
            []                      -> bug "No scopes!"
            (scopeID, names) : rest -> (scopeID + 1, names) : rest
        return result

    bindName btype name = NameResolve $ do
        context <- M.get
        when (Map.member name (snd (assert (head context)))) $ do
            lift (M.throwE (NameWouldShadow name (map fst context)))
        M.put $ case context of
            []                      -> bug "No scopes!"
            (scopeID, names) : rest -> (scopeID, Map.insert name btype names) : rest
        return (Name (map fst (assert (tail context))) name btype)

resolveNames :: AST Text -> Either Error (AST Name)
resolveNames ast = M.runExcept (M.evalStateT (runNameResolve (resolveNamesM ast)) [(0, Map.empty)])

resolveNamesM :: NameResolveM m => AST Text -> m (AST Name)
resolveNamesM = mapM resolveStatementM

resolveStatementM :: NameResolveM m => AST.Statement Text -> m (AST.Statement Name)
resolveStatementM = \case
    AST.Binding btype name expr -> do
        resolvedExpr <- resolveExpressionM expr
        fullName <- bindName btype name
        return (AST.Binding btype fullName resolvedExpr)
    AST.Assign name expr -> do
        resolvedName <- lookupName name
        resolvedExpr <- resolveExpressionM expr
        return (AST.Assign resolvedName resolvedExpr)
    AST.IfThen expr body -> do
        resolvedExpr <- resolveExpressionM expr
        resolvedBody <- inNewScope (resolveNamesM body)
        return (AST.IfThen resolvedExpr resolvedBody)
    AST.IfThenElse expr body1 body2 -> do
        resolvedExpr  <- resolveExpressionM expr
        resolvedBody1 <- inNewScope (resolveNamesM body1)
        resolvedBody2 <- inNewScope (resolveNamesM body2)
        return (AST.IfThenElse resolvedExpr resolvedBody1 resolvedBody2)
    AST.Forever body -> do
        resolvedBody <- inNewScope (resolveNamesM body)
        return (AST.Forever resolvedBody)
    AST.While expr body -> do
        resolvedExpr <- resolveExpressionM expr
        resolvedBody <- inNewScope (resolveNamesM body)
        return (AST.While resolvedExpr resolvedBody)
    AST.Return maybeExpr -> do
        resolvedMaybeExpr <- mapM resolveExpressionM maybeExpr
        return (AST.Return resolvedMaybeExpr)
    AST.Break -> do
        return AST.Break
    AST.Write expr -> do
        resolvedExpr <- resolveExpressionM expr
        return (AST.Write resolvedExpr)

resolveExpressionM :: NameResolveM m => AST.Expression Text -> m (AST.Expression Name)
resolveExpressionM = \case
    AST.Name name -> do
        resolvedName <- lookupName name
        return (AST.Name resolvedName)
    AST.Literal n -> do
        return (AST.Literal n)
    AST.UnaryOperator op expr -> do
        resolvedExpr <- resolveExpressionM expr
        return (AST.UnaryOperator op resolvedExpr)
    AST.BinaryOperator expr1 op expr2 -> do
        resolvedExpr1 <- resolveExpressionM expr1
        resolvedExpr2 <- resolveExpressionM expr2
        return (AST.BinaryOperator resolvedExpr1 op resolvedExpr2)
    AST.Read -> do
        return AST.Read

isWellFormed :: AST Name -> Bool
isWellFormed = todo

lookupPath :: Path -> AST name -> Maybe (AST name)
lookupPath = todo
