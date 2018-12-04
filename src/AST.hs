{-# LANGUAGE RecursiveDo #-} -- needed for Earley

module AST (Expression (..), BindingType (..), Statement (..), Block (..), Argument (..), Function (..), AST, Error (..), parse, RenderName (..)) where

import MyPrelude

import qualified Text.Earley as E

import qualified Pretty as P
import qualified Token  as T

import Pretty (Render, render)


----------------------------------------------------------------------------- types

data Expression a name
    = Named
        name
    | Call
        name
        [NodeWith Expression a name]
    | NumberLiteral
        Integer
    | TextLiteral
        Text
    | UnaryOperator
        UnaryOperator
        (NodeWith Expression a name)
    | BinaryOperator
        (NodeWith Expression a name)
        BinaryOperator
        (NodeWith Expression a name)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data BindingType
    = Let
    | Var
    deriving (Generic, Eq, Show)

data Statement a name
    = Expression
        (NodeWith Expression a name)
    | Binding
        BindingType
        name
        (NodeWith Expression a name)
    | Assign
        name
        (NodeWith Expression a name)
    | IfThen
        (NodeWith Expression a name)
        (NodeWith Block      a name)
    | IfThenElse
        (NodeWith Expression a name)
        (NodeWith Block      a name)
        (NodeWith Block      a name)
    | Forever
        (NodeWith Block      a name)
    | While
        (NodeWith Expression a name)
        (NodeWith Block      a name)
    | Return
        name -- return and break refer to the `exitTarget` in `Block`; these are "phantom names", not present in the source code
        (Maybe (NodeWith Expression a name))
    | Break
        name -- see above
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Block a name = Block {
    exitTarget :: Maybe name, -- "phantom", see above
    statements :: [NodeWith Statement a name]
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Argument a name = Argument {
    argumentName :: name, -- TODO `With a name`?
    argumentType :: name
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Function a name = Function {
    functionName :: name,
    arguments    :: [NodeWith Argument a name],
    returns      :: Maybe name, -- TODO `With a name`?
    body         :: NodeWith Block a name
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

type Located node = NodeWith node Loc


----------------------------------------------------------------------------- parsing

type Expected         = Text
type Prod      r      = Compose (E.Prod r Expected (With Loc T.Token)) (With Loc)
type Grammar   r node = E.Grammar r (Prod r (Located node Text))

token :: T.Token -> Prod r ()
token = unused . Compose . E.token . pure

keyword :: T.Keyword -> Prod r ()
keyword = token . T.Keyword

terminal :: (T.Token -> Maybe a) -> Prod r a
terminal f = Compose (E.terminal (\(With loc a) -> fmap (With loc) (f a)))

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
located :: Prod r (node Loc Text) -> Prod r (Located node Text)
located = Compose . fmap dupLocated . getCompose where
    dupLocated node = With (getWith node) (NodeWith node)

nodeRule :: Prod r (node Loc Text) -> Grammar r node
nodeRule = fmap Compose . E.rule . getCompose . located

ruleCases :: [Prod r (node Loc Text)] -> Grammar r node
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

data BinaryOperationList a name
    = SingleExpression (NodeWith Expression a name)
    | BinaryOperation  (NodeWith Expression a name) BinaryOperator (Located BinaryOperationList name)

resolvePrecedences :: Located BinaryOperationList Text -> Located Expression Text
resolvePrecedences binaryOperationList = todo {-finalResult where
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
        other -> other-}

expressionGrammar :: Grammar r Expression
expressionGrammar = mdo
    atom     <- ruleCases [liftA1 Named         (tokenConstructor @"Name"),
                           liftA1 NumberLiteral (tokenConstructor @"Number"),
                           liftA1 TextLiteral   (tokenConstructor @"Text"),
                           liftA2 Call          (tokenConstructor @"Name") (bracketed T.Round (separatedBy T.Comma expression)),
                           liftA1 nodeWithout (bracketed T.Round expression)]
    unary    <- ruleCases [liftA2 UnaryOperator (tokenConstructor @"UnaryOperator") atom,
                           liftA1 nodeWithout atom] -- TODO is there any nicer way to do this
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


type AST a name = [NodeWith Function a name]

data Error
    = Invalid Int [Expected] [With Loc T.Token]
    | Ambiguous [AST Loc Text]
    deriving (Generic)

deriving instance Show Error

parse :: [With Loc T.Token] -> Either Error (AST Loc Text)
parse = checkResult . E.fullParses parser where
    parser = E.parser (liftM oneOrMore (fmap (fmap unWith . getCompose) functionGrammar))
    checkResult = \case
        ([], E.Report a b c) ->
            Left (Invalid a b c)
        ([one], _) ->
            Right one
        (more,  _) ->
            Left (Ambiguous more)





----------------------------------------------------------------------------- pretty-printing

renderBlock :: RenderName name => NodeWith Block a name -> P.Document
renderBlock block = P.braces (P.nest 4 (P.hardline ++ render block) ++ P.hardline)

class RenderName name where
    renderName :: P.DefinitionOrUse -> name -> P.Document

instance RenderName Text where
    renderName defOrUse name = P.note (P.Identifier (P.IdentInfo name defOrUse P.Unknown False)) (P.pretty name)

instance RenderName name => Render (Expression a name) where
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

instance RenderName name => Render (Statement a name) where
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

instance RenderName name => Render (Block a name) where
    render Block { statements } = mconcat (P.punctuate P.hardline (map render statements))

instance RenderName name => Render (Argument a name) where
    render Argument { argumentName, argumentType } = renderName P.Definition argumentName ++ P.colon ++ " " ++ renderName P.Use argumentType

instance RenderName name => Render (Function a name) where
    render Function { functionName, arguments, returns, body } = renderedHead ++ renderedArguments ++ renderedReturns ++ renderedBody where
        renderedHead      = P.keyword "function" ++ " " ++ renderName P.Definition functionName
        renderedArguments = P.parens (P.hsep (P.punctuate "," (map render arguments)))
        renderedReturns   = maybe "" (\returnType -> " " ++ P.keyword "returns" ++ " " ++ renderName P.Use returnType) returns
        renderedBody      = P.hardline ++ renderBlock body

instance RenderName name => Render (AST a name) where
    render = mconcat . P.punctuate P.hardline . map render
