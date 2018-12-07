{-# LANGUAGE RecursiveDo #-} -- needed for Earley

module AST (Type (..), Expression (..), BindingType (..), Statement (..), Block (..), Argument (..), Function (..), AST, Error (..), parse, RenderName (..)) where

import MyPrelude

import qualified Text.Earley as E

import qualified Pretty as P
import qualified Token  as T

import Pretty (Render, render)


----------------------------------------------------------------------------- types

data Type metadata name
    = NamedType
        name
    | FunctionType
        [NodeWith Type metadata name]
        (NodeWith Type metadata name)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Expression metadata name
    = Named
        name
    | Call
        (NodeWith Expression metadata name)
        [NodeWith Expression metadata name]
    | NumberLiteral
        Integer
    | TextLiteral
        Text
    | UnaryOperator
        UnaryOperator
        (NodeWith Expression metadata name)
    | BinaryOperator
        (NodeWith Expression metadata name)
        BinaryOperator
        (NodeWith Expression metadata name)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data BindingType
    = Let
    | Var
    deriving (Generic, Eq, Show)

data Statement metadata name
    = Expression
        (NodeWith Expression metadata name)
    | Binding
        BindingType
        name
        (NodeWith Expression metadata name)
    | Assign
        name
        (NodeWith Expression metadata name)
    | IfThen
        (NodeWith Expression metadata name)
        (NodeWith Block      metadata name)
    | IfThenElse
        (NodeWith Expression metadata name)
        (NodeWith Block      metadata name)
        (NodeWith Block      metadata name)
    | Forever
        (NodeWith Block      metadata name)
    | While
        (NodeWith Expression metadata name)
        (NodeWith Block      metadata name)
    | Return
        name -- return and break refer to the `exitTarget` in `Block`; these are "phantom names", not present in the source code
        (Maybe (NodeWith Expression metadata name))
    | Break
        name -- see above
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Block metadata name = Block {
    exitTarget :: Maybe name, -- "phantom", see above
    statements :: [NodeWith Statement metadata name]
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Argument metadata name = Argument {
    argumentName :: name,
    argumentType :: NodeWith Type metadata name
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Function metadata name = Function {
    functionName :: name,
    arguments    :: [NodeWith Argument metadata name],
    returns      :: Maybe (NodeWith Type metadata name),
    body         :: NodeWith Block metadata name
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)



----------------------------------------------------------------------------- parsing

type Expected         = Text
type Prod      r      = Compose (E.Prod r Expected (With Loc T.Token)) (With Loc)
type Grammar   r node = E.Grammar r (Prod r (node Loc Text))

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
located :: Prod r (node Loc Text) -> Prod r (NodeWith node Loc Text)
located = Compose . fmap dupLocated . getCompose where
    dupLocated node = With (getMetadata node) (NodeWith node)

nodeRule :: Prod r (node Loc Text) -> Grammar r node
nodeRule = fmap Compose . E.rule . getCompose

locatedNode :: Prod r (node Loc Text) -> Grammar r (NodeWith node)
locatedNode = nodeRule . located

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

data BinaryOperationList metadata name
    = SingleExpression (NodeWith Expression metadata name)
    | BinaryOperation  (NodeWith Expression metadata name) BinaryOperator (BinaryOperationList metadata name)
    deriving Show

resolvePrecedences :: BinaryOperationList Loc Text -> NodeWith Expression Loc Text
resolvePrecedences binaryOperationList = finalResult where
    finalResult = case allPrecedencesResolved of
        SingleExpression expr -> expr
        list                  -> bug ("Unresolved binary operator precedence: " ++ prettyShow list)
    allPrecedencesResolved = foldl' resolveOnePrecedenceLevel binaryOperationList precedenceGroups
    resolveOnePrecedenceLevel binOpList precedenceGroup = case binOpList of
        BinaryOperation expr1 op1 (BinaryOperation expr2 op2 rest)
            | elem op1 precedenceGroup -> resolveOnePrecedenceLevel (BinaryOperation (locatedBinaryOperator expr1 op1 expr2) op2 rest) precedenceGroup
            | otherwise                -> BinaryOperation expr1 op1 (resolveOnePrecedenceLevel (BinaryOperation expr2 op2 rest) precedenceGroup)
        BinaryOperation expr1 op (SingleExpression expr2)
            | elem op  precedenceGroup -> SingleExpression (locatedBinaryOperator expr1 op expr2)
        other -> other
    locatedBinaryOperator expr1 op expr2 = NodeWith (With combinedLoc (BinaryOperator expr1 op expr2)) where
        combinedLoc = mconcat (map nodeMetadata [expr1, expr2])

expressionGrammar :: Grammar r (NodeWith Expression)
expressionGrammar = mdo
    atom <- (locatedNode . oneOf)
        [
            liftA1 Named         (tokenConstructor @"Name"),
            liftA1 NumberLiteral (tokenConstructor @"Number"),
            liftA1 TextLiteral   (tokenConstructor @"Text"),
            liftA1 nodeWithout   (bracketed T.Round expression)
        ]
    call <- (locatedNode . oneOf)
        [
            liftA2 Call        call (bracketed T.Round (separatedBy T.Comma expression)),
            liftA1 nodeWithout atom
        ]
    unary <- (locatedNode . oneOf)
        [
            liftA2 UnaryOperator (tokenConstructor @"UnaryOperator") unary,
            liftA1 nodeWithout call
        ]
    binaries <- (nodeRule . oneOf)
        [
            liftA3 BinaryOperation  unary (tokenConstructor @"BinaryOperator") binaries,
            liftA1 SingleExpression unary
        ]
    let expression = liftA1 resolvePrecedences binaries
    return expression

blockGrammar :: Grammar r (NodeWith Block)
blockGrammar = mdo
    expression <- expressionGrammar
    -----------------------------------------------------------
    binding <- locatedNode do
        letvar <- terminal (\case T.Keyword T.K_let -> Just Let; T.Keyword T.K_var -> Just Var; _ -> Nothing) -- TODO prism?
        name   <- tokenConstructor @"Name"
        token T.EqualsSign
        rhs    <- expression
        token T.Semicolon
        return (Binding letvar name rhs)
    assign <- locatedNode do
        lhs <- tokenConstructor @"Name"
        token T.EqualsSign
        rhs <- expression
        token T.Semicolon
        return (Assign lhs rhs)
    ifthen <- locatedNode do
        keyword T.K_if
        cond <- expression
        body <- block
        return (IfThen cond body)
    ifthenelse <- locatedNode do
        keyword T.K_if
        cond  <- expression
        body1 <- block
        keyword T.K_else
        body2 <- block
        return (IfThenElse cond body1 body2)
    forever <- locatedNode do
        keyword T.K_forever
        body <- block
        return (Forever (mapNode (set (field @"exitTarget") (Just "break")) body))
    while <- locatedNode do
        keyword T.K_while
        cond <- expression
        body <- block
        return (While cond (mapNode (set (field @"exitTarget") (Just "break")) body))
    ret <- locatedNode do
        keyword T.K_return
        arg <- liftA1 head (zeroOrOne expression)
        token T.Semicolon
        return (Return "return" arg)
    break <- locatedNode do
        keyword T.K_break
        token T.Semicolon
        return (Break "break")
    exprStatement <- locatedNode do
        expr <- expression
        token T.Semicolon
        return (Expression expr)
    -----------------------------------------------------
    statement <- nodeRule (oneOf [binding, assign, ifthen, ifthenelse, forever, while, ret, break, exprStatement])
    block <- locatedNode do
        statements <- bracketed T.Curly (oneOrMore statement)
        return Block { exitTarget = Nothing, statements }
    return block

typeGrammar :: Grammar r (NodeWith Type)
typeGrammar = mdo
    functionType <- nodeRule do
        keyword T.K_function
        parameters <- bracketed T.Round (separatedBy T.Comma type')
        keyword T.K_returns
        returns <- type'
        return (FunctionType parameters returns)
    type' <- locatedNode (oneOf [liftA1 NamedType (tokenConstructor @"Name"), functionType])
    return type'

functionGrammar :: Grammar r (NodeWith Function)
functionGrammar = do
    block <- blockGrammar
    type' <- typeGrammar
    argument <- locatedNode do
        argumentName <- tokenConstructor @"Name"
        token T.Colon
        argumentType <- type'
        return Argument { argumentName, argumentType }
    locatedNode do
        keyword T.K_function
        functionName <- tokenConstructor @"Name"
        arguments    <- bracketed T.Round (separatedBy T.Comma argument)
        returns      <- liftA1 head (zeroOrOne (keyword T.K_returns `followedBy` type'))
        body         <- block
        return Function { functionName, arguments, returns, body = mapNode (set (field @"exitTarget") (Just "return")) body }

type AST metadata name = [NodeWith Function metadata name]

data Error
    = Invalid Int [Expected] [With Loc T.Token]
    | Ambiguous [AST Loc Text]
    deriving (Generic, Show)

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

renderBlock :: RenderName name => NodeWith Block metadata name -> P.Document
renderBlock block =
    P.braces (P.nest 4 (P.hardline ++ render block) ++ P.hardline)

class RenderName name where
    renderName :: P.DefinitionOrUse -> name -> P.Document

instance RenderName Text where
    renderName defOrUse name =
        P.note (P.Identifier (P.IdentInfo name defOrUse P.Unknown False)) (P.pretty name)

instance RenderName name => Render (Type metadata name) where
    listSeparator = ", "
    render = \case
        NamedType name ->
            renderName P.Use name
        FunctionType parameters returns ->
            P.keyword "function" ++ P.parens (render parameters) ++ " " ++ P.keyword "returns" ++ " " ++ render returns

instance RenderName name => Render (Expression metadata name) where
    listSeparator = ", "
    render = \case
        Named name ->
            renderName P.Use name
        Call fn args ->
            render fn ++ P.parens (render args)
        NumberLiteral number->
            P.number number
        TextLiteral text ->
            P.string text
        UnaryOperator op expr->
            P.unaryOperator op ++ render expr
        BinaryOperator expr1 op expr2 ->
            render expr1 ++ " " ++ P.binaryOperator op ++ " " ++ render expr2

instance Render BindingType where
    render = P.keyword . \case
        Let -> "let"
        Var -> "var"

instance RenderName name => Render (Statement metadata name) where
    render = \case
        Binding btype name expr ->
            render btype ++ " " ++ renderName P.Definition name ++ " " ++ P.defineEquals ++ " " ++ render expr ++ P.semicolon
        Assign name expr ->
            renderName P.Use name ++ " " ++ P.assignEquals ++ " " ++ render expr ++ P.semicolon
        IfThen expr block ->
            P.keyword "if" ++ " " ++ render expr ++ " " ++ renderBlock block
        IfThenElse expr block1 block2 ->
            render (IfThen expr block1) ++ " " ++ P.keyword "else" ++ " " ++ renderBlock block2
        Forever block ->
            P.keyword "forever" ++ " " ++ renderBlock block
        While expr block ->
            P.keyword "while" ++ " " ++ render expr ++ " " ++ renderBlock block
        Return _ maybeExpr ->
            P.keyword "return" ++ (maybe "" (\expr -> " " ++ render expr) maybeExpr) ++ P.semicolon
        Break _ ->
            P.keyword "break" ++ P.semicolon
        Expression expr ->
            render expr ++ P.semicolon

instance RenderName name => Render (Block metadata name) where
    render Block { statements } = render statements

instance RenderName name => Render (Argument metadata name) where
    listSeparator = ", "
    render Argument { argumentName, argumentType } =
        renderName P.Definition argumentName ++ P.colon ++ " " ++ render argumentType

instance RenderName name => Render (Function metadata name) where
    render Function { functionName, arguments, returns, body } =
        renderedHead ++ renderedArguments ++ renderedReturns ++ renderedBody where
            renderedHead      = P.keyword "function" ++ " " ++ renderName P.Definition functionName
            renderedArguments = P.parens (render arguments)
            renderedReturns   = maybe "" (\returnType -> " " ++ P.keyword "returns" ++ " " ++ render returnType) returns
            renderedBody      = P.hardline ++ renderBlock body
