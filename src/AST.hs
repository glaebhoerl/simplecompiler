{-# LANGUAGE RecursiveDo, QuantifiedConstraints #-} -- needed for Earley

module AST (Expression (..), BindingType (..), Statement (..), Block (..), Argument (..), Function (..), AST, Error (..), parse, RenderName (..)) where

import MyPrelude

import Data.Functor.Compose
import Data.Functor.Classes (Eq1, Show1)
import GHC.Exts (Constraint)

import qualified Data.Loc    as Loc
import qualified Text.Earley as E

import qualified Pretty as P
import qualified Token  as T

import Pretty (Render, render)



----------------------------------------------------------------------------- types

type ComposedNode f node  = Compose f (node f)

type ComposedExpression f = ComposedNode f Expression
type ComposedStatement  f = ComposedNode f Statement
type ComposedBlock      f = ComposedNode f Block
type ComposedArgument   f = ComposedNode f Argument
type ComposedFunction   f = ComposedNode f Function

data Expression f name
    = Named
        name
    | Call
        name
        [ComposedExpression f name]
    | NumberLiteral
        Integer
    | TextLiteral
        Text
    | UnaryOperator
        UnaryOperator
        (ComposedExpression f name)
    | BinaryOperator
        (ComposedExpression f name)
        BinaryOperator
        (ComposedExpression f name)
    deriving (Generic, Functor, Foldable, Traversable)

data BindingType
    = Let
    | Var
    deriving (Generic, Eq, Show)

data Statement f name
    = Expression
        (ComposedExpression f name)
    | Binding
        BindingType
        name
        (ComposedExpression f name)
    | Assign
        name
        (ComposedExpression f name)
    | IfThen
        (ComposedExpression f name)
        (ComposedBlock      f name)
    | IfThenElse
        (ComposedExpression f name)
        (ComposedBlock      f name)
        (ComposedBlock      f name)
    | Forever
        (ComposedBlock      f name)
    | While
        (ComposedExpression f name)
        (ComposedBlock      f name)
    | Return
        name -- return and break refer to the `exitTarget` in `Block`; these are "phantom names", not present in the source code
        (Maybe (ComposedExpression f name))
    | Break
        name -- see above
    deriving (Generic, Functor, Foldable, Traversable)

data Block f name = Block {
    exitTarget :: Maybe name, -- "phantom", see above
    statements :: [ComposedStatement f name]
} deriving (Generic, Functor, Foldable, Traversable)

data Argument f name = Argument {
    argumentName :: name, -- TODO maybe use `f name` here
    argumentType :: name
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Function f name = Function {
    functionName :: name,
    arguments    :: [ComposedArgument f name],
    returns      :: Maybe name, -- TODO ComposedNode Identity f?
    body         :: ComposedBlock f name
} deriving (Generic, Functor, Foldable, Traversable)

type Forall c f = (forall a. c a => c (f a) :: Constraint)

type EqF   f = Forall Eq   f
type ShowF f = Forall Show f

deriving instance (Eq1   f, Eq   a) => Eq   (Expression f a)
deriving instance (Eq1   f, Eq   a) => Eq   (Statement  f a)
deriving instance (Eq1   f, Eq   a) => Eq   (Block      f a)
deriving instance (Eq1   f, Eq   a) => Eq   (Function   f a)
deriving instance (Show1 f, Show a) => Show (Expression f a)
deriving instance (Show1 f, Show a) => Show (Statement  f a)
deriving instance (Show1 f, Show a) => Show (Block      f a)
deriving instance (Show1 f, Show a) => Show (Function   f a)


data Located a = At {
    location :: Loc.SrcLoc,
    retrieve :: a
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Applicative Located where
    pure = At mempty
    liftA2 f (At l1 a1) (At l2 a2) = At (l1 ++ l2) (f a1 a2)

type LocatedExpression = ComposedExpression Located
type LocatedStatement  = ComposedStatement  Located
type LocatedBlock      = ComposedBlock      Located
type LocatedArgument   = ComposedArgument   Located
type LocatedFunction   = ComposedFunction   Located


----------------------------------------------------------------------------- parsing

type Expected         = Text
type Prod      r      = Compose (E.Prod r Expected (Located T.Token)) Located
type Grammar   r node = E.Grammar r (Prod r (ComposedNode Located node Text))

recompose :: Functor f => Compose f g (h a) -> f (Compose g h a)
recompose (Compose x) = fmap Compose x

token :: T.Token -> Prod r ()
token = unused . Compose . E.token . pure

keyword :: T.Keyword -> Prod r ()
keyword = token . T.Keyword

terminal :: (T.Token -> Maybe a) -> Prod r a
terminal f = Compose (E.terminal (\(At loc a) -> fmap (At loc) (f a)))

tokenConstructor :: forall name inner r. AsConstructor' name T.Token inner => Prod r inner
tokenConstructor = terminal (match @name)

bracketed :: T.BracketKind -> Prod r output -> Prod r output
bracketed kind inner = do
    token (T.Bracket' (T.Bracket kind T.Open))
    output <- inner
    token (T.Bracket' (T.Bracket kind T.Close))
    return output

separatedBy :: T.Token -> Prod r output -> Prod r [output]
separatedBy t element = oneOf [pure [], liftA2 prepend element (zeroOrMore (token t `followedBy` element))]

followedBy :: Prod r a -> Prod r b -> Prod r b
followedBy = (*>)

-- This may seem somewhat surprising -- why do we need to /duplicate/ the location info? Doesn't the Applicative instance handle this for us?
-- The explanation is that Applicative only handles combining the sublocations into the location of the final result --
--   but we don't just want the location of the whole tree, we also want the locations of all the sub-nodes!
-- So this takes a snapshot of the location for the subnode, and also lets `Applicative` go on combining it into the location of the parent node.
located :: Prod r (node Located Text) -> Prod r (ComposedNode Located node Text)
located = Compose . fmap dupLocated . getCompose where
    dupLocated node = At (location node) (Compose node)

nodeRule :: Prod r (node Located Text) -> Grammar r node
nodeRule = fmap Compose . E.rule . getCompose . located

ruleCases :: [Prod r (node Located Text)] -> Grammar r node
ruleCases cases = nodeRule (oneOf cases)

-- from tightest to loosest; operators within a group have equal precedence
precedenceGroups :: [[BinaryOperator]]
precedenceGroups = assert (justIf isWellFormed listOfGroups) where
    isWellFormed = all exactly1 (enumerate :: [BinaryOperator]) && not (any null listOfGroups)
    exactly1 op  = length (filter (== op) (concat listOfGroups)) == 1
    listOfGroups =
        [map ArithmeticOperator [Mul, Div, Mod],
         map ArithmeticOperator [Add, Sub],
         map ComparisonOperator [Less, LessEqual, Greater, GreaterEqual],
         map ComparisonOperator [Equal, NotEqual],
         map LogicalOperator    [And],
         map LogicalOperator    [Or]]

data BinaryOperationList f name
    = SingleExpression (ComposedExpression f name)
    | BinaryOperation  (ComposedExpression f name) BinaryOperator (ComposedNode Located BinaryOperationList name)

resolvePrecedences :: ComposedNode Located BinaryOperationList Text -> LocatedExpression Text
resolvePrecedences binaryOperationList = finalResult where
    finalResult = case allPrecedencesResolved of
        SingleExpression expr -> expr
        list                  -> bug ("Unresolved binary operator precedence: " ++ prettyShow list)
    allPrecedencesResolved = foldl' resolveOnePrecedenceLevel binaryOperationList precedenceGroups
    resolveOnePrecedenceLevel binOpList precedenceGroup = case binOpList of
        BinaryOperation expr1 op1 (BinaryOperation expr2 op2 rest)
            | elem op1 precedenceGroup -> resolveOnePrecedenceLevel (BinaryOperation (BinaryOperator expr1 op1 expr2) op2 rest) precedenceGroup
            | otherwise                -> BinaryOperation expr1 op1 (resolveOnePrecedenceLevel (BinaryOperation expr2 op2 rest) precedenceGroup)
        BinaryOperation expr1 op (SingleExpression expr2)
            | elem op  precedenceGroup -> SingleExpression (BinaryOperator expr1 op expr2)
        other -> other

expressionGrammar :: Grammar r Expression
expressionGrammar = mdo
    atom     <- ruleCases [liftA1 Named         (tokenConstructor @"Name"),
                           liftA1 NumberLiteral (tokenConstructor @"Number"),
                           liftA1 TextLiteral   (tokenConstructor @"Text"),
                           liftA2 Call          (tokenConstructor @"Name") (bracketed T.Round (separatedBy T.Comma expression)),
                           liftA1 (retrieve . getCompose) (bracketed T.Round expression)]
    unary    <- ruleCases [liftA2 UnaryOperator (tokenConstructor @"UnaryOperator") atom,
                           liftA1 (retrieve . getCompose) atom] -- TODO is there any nicer way to do this
    binaries <- ruleCases [liftA3 BinaryOperation  unary (tokenConstructor @"BinaryOperator") binaries,
                           liftA1 SingleExpression unary]
    let expression = liftA1 resolvePrecedences binaries
    return expression

blockGrammar :: Grammar r Block
blockGrammar = mdo
    expression <- expressionGrammar
    -----------------------------------------------------------
    binding <- nodeRule do
        letvar <- terminal (\case T.Keyword T.K_let -> Just Let; T.Keyword T.K_var -> Just Var; _ -> Nothing) -- TODO prism?
        name   <- tokenConstructor @"Name"
        token T.EqualsSign
        rhs    <- expression
        token T.Semicolon
        return (Binding letvar name rhs)
    assign <- nodeRule do
        lhs <- tokenConstructor @"Name"
        token T.EqualsSign
        rhs <- expression
        token T.Semicolon
        return (Assign lhs rhs)
    ifthen <- nodeRule do
        keyword T.K_if
        cond <- expression
        body <- block
        return (IfThen cond body)
    ifthenelse <- nodeRule do
        keyword T.K_if
        cond  <- expression
        body1 <- block
        keyword T.K_else
        body2 <- block
        return (IfThenElse cond body1 body2)
    forever <- nodeRule do
        keyword T.K_forever
        body <- block
        return (Forever body)
    while <- nodeRule do
        keyword T.K_while
        cond <- expression
        body <- block
        return (While cond body)
    ret <- nodeRule do
        keyword T.K_return
        arg <- liftA1 head (zeroOrOne expression)
        token T.Semicolon
        return (Return "return" arg)
    break <- nodeRule do
        keyword T.K_break
        token T.Semicolon
        return (Break "break")
    exprStatement <- nodeRule do
        expr <- expression
        token T.Semicolon
        return (Expression expr)
    -----------------------------------------------------
    let statement = oneOf [binding, assign, ifthen, ifthenelse, forever, while, ret, break, exprStatement] -- TODO this might need to be a `rule` to avoid <<loop>>!
    block <- nodeRule do
        statements <- bracketed T.Curly (oneOrMore statement)
        return Block { exitTarget = Nothing, statements }
    return block

functionGrammar :: Grammar r Function
functionGrammar = do
    block <- blockGrammar
    argument <- nodeRule do
        argumentName <- tokenConstructor @"Name"
        token T.Colon
        argumentType <- tokenConstructor @"Name"
        return Argument { argumentName, argumentType }
    nodeRule do
        keyword T.K_function
        functionName <- tokenConstructor @"Name"
        arguments    <- bracketed T.Round (separatedBy T.Comma argument)
        returns      <- liftA1 head (zeroOrOne (keyword T.K_returns `followedBy` tokenConstructor @"Name"))
        body         <- block
        return Function { functionName, arguments, returns, body }


type AST name = [LocatedFunction name]

data Error
    = Invalid Int [Expected] [Located T.Token]
    | Ambiguous [AST Text]
    deriving (Generic)

deriving instance Show Error

parse :: [Loc.L T.Token] -> Either Error (AST Text)
parse = checkResult . E.fullParses parser . map convertLocation where
    convertLocation (Loc.L l a) = At (Loc.SrcLoc l) a -- TODO use `Located` everywhere instead
    parser = E.parser (liftM oneOrMore (fmap (fmap retrieve . getCompose) functionGrammar))
    checkResult = \case
        ([], E.Report a b c) ->
            Left (Invalid a b c)
        ([one], _) ->
            Right one
        (more,  _) ->
            Left (Ambiguous more)





----------------------------------------------------------------------------- pretty-printing

type RenderF f = Forall Render f

instance Render a => Render (Located a) where
    render = render . retrieve

instance Render (f (g a)) => Render (Compose f g a) where
    render = render . getCompose

renderBlock :: (RenderF f, RenderName name) => ComposedBlock f name -> P.Document
renderBlock block = P.braces (P.nest 4 (P.hardline ++ render block) ++ P.hardline)

class RenderName name where
    renderName :: P.DefinitionOrUse -> name -> P.Document

instance RenderName Text where
    renderName defOrUse name = P.note (P.Identifier (P.IdentInfo name defOrUse P.Unknown False)) (P.pretty name)

instance (RenderF f, RenderName name) => Render (Expression f name) where
    render = \case
        Named          name           -> renderName P.Use name
        Call           name args      -> renderName P.Use name ++ P.parens (P.hsep (P.punctuate "," (map render args)))
        NumberLiteral  number         -> P.number number
        TextLiteral    text           -> P.string text
        UnaryOperator  op expr        -> P.unaryOperator op ++ render expr
        BinaryOperator expr1 op expr2 -> render expr1 ++ " " ++ P.binaryOperator op ++ " " ++ render expr2

instance Render BindingType where
    render = P.keyword . \case
        Let -> "let"
        Var -> "var"

instance (RenderF f, RenderName name) => Render (Statement f name) where
    render = \case
        Binding    btype name expr    -> render btype ++ " " ++ renderName P.Definition name ++ " " ++ P.defineEquals ++ " " ++ render expr ++ P.semicolon
        Assign     name expr          -> renderName P.Use name ++ " " ++ P.assignEquals ++ " " ++ render expr ++ P.semicolon
        IfThen     expr block         -> P.keyword "if"      ++ " " ++ render expr ++ " " ++ renderBlock block
        IfThenElse expr block1 block2 -> render (IfThen expr block1) ++ " " ++ P.keyword "else" ++ " " ++ renderBlock block2
        Forever    block              -> P.keyword "forever" ++ " " ++ renderBlock block
        While      expr block         -> P.keyword "while"   ++ " " ++ render expr ++ " " ++ renderBlock block
        Return     _    maybeExpr     -> P.keyword "return"  ++ (maybe "" (\expr -> " " ++ render expr) maybeExpr) ++ P.semicolon
        Break      _                  -> P.keyword "break"   ++ P.semicolon
        Expression expr               -> render expr       ++ P.semicolon

instance (RenderF f, RenderName name) => Render (Block f name) where
    render Block { statements } = mconcat (P.punctuate P.hardline (map render statements))

instance (RenderF f, RenderName name) => Render (Argument f name) where
    render Argument { argumentName, argumentType } = renderName P.Definition argumentName ++ P.colon ++ " " ++ renderName P.Use argumentType

instance (RenderF f, RenderName name) => Render (Function f name) where
    render Function { functionName, arguments, returns, body } = renderedHead ++ renderedArguments ++ renderedReturns ++ renderedBody where
        renderedHead      = P.keyword "function" ++ " " ++ renderName P.Definition functionName
        renderedArguments = P.parens (P.hsep (P.punctuate "," (map render arguments)))
        renderedReturns   = maybe "" (\returnType -> " " ++ P.keyword "returns" ++ " " ++ renderName P.Use returnType) returns
        renderedBody      = P.hardline ++ renderBlock body

instance RenderName name => Render (AST name) where
    render = mconcat . P.punctuate P.hardline . map render
