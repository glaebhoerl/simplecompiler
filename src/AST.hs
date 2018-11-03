{-# LANGUAGE AllowAmbiguousTypes #-}

module AST (Expression (..), BindingType (..), Statement (..), Block (..), Argument (..), Function (..), AST, Error (..), parse, RenderName (..)) where

import MyPrelude

import qualified Text.Earley as E

import qualified Pretty as P
import qualified Token  as T



----------------------------------------------------------------------------- types

data Expression name
    = Named          !name
    | Call           !name ![Expression name]
    | NumberLiteral  !Integer
    | TextLiteral    !Text
    | UnaryOperator  !UnaryOperator     !(Expression name)
    | BinaryOperator !(Expression name) !BinaryOperator !(Expression name)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data BindingType
    = Let
    | Var
    deriving (Generic, Eq, Show)

data Statement name
    = Binding    !BindingType       !name              !(Expression name)
    | Assign     !name              !(Expression name)
    | IfThen     !(Expression name) !(Block name)
    | IfThenElse !(Expression name) !(Block name)      !(Block name)
    | Forever    !(Block name)
    | While      !(Expression name) !(Block name)
    | Return     !(Maybe (Expression name))
    | Break
    | Expression !(Expression name)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

newtype Block name = Block {
    statements :: [Statement name]
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Argument name = Argument {
    argumentName :: !name,
    argumentType :: !name
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data Function name = Function {
    functionName :: !name,
    arguments    :: ![Argument name],
    returns      :: !(Maybe name),
    body         :: !(Block name)
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)




----------------------------------------------------------------------------- parsing

type Expected = Text

type Prod    r output = E.Prod r Expected T.Token output
type Grammar r output = E.Grammar r (Prod r output)

token :: T.Token -> Prod r ()
token t = unused (E.token t)

keyword :: T.Keyword -> Prod r ()
keyword kw = token (T.Keyword kw)

tokenConstructor :: forall name inner r. AsConstructor' name T.Token inner => Prod r inner
tokenConstructor = E.terminal (getWhen (constructor @name))

bracketed :: T.BracketKind -> Prod r output -> Prod r output
bracketed kind inner = do
    token (T.Bracket' (T.Bracket kind T.Open))
    output <- inner
    token (T.Bracket' (T.Bracket kind T.Close))
    return output

separatedBy :: [T.Token] -> Prod r output -> Prod r [output]
separatedBy tokens element = oneOf [pure [], liftA2 prepend element (zeroOrMore ((E.list tokens) `followedBy` element))]

followedBy :: Prod r a -> Prod r b -> Prod r b
followedBy = (*>)

ruleCases :: [Prod r output] -> Grammar r output
ruleCases cases = E.rule (oneOf cases)

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

data BinaryOperationList
    = SingleExpression (Expression Text)
    | BinaryOperation  (Expression Text) BinaryOperator BinaryOperationList
    deriving Show

resolvePrecedences :: BinaryOperationList -> Expression Text
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

expressionGrammar :: Grammar r (Expression Text)
expressionGrammar = mdo
    atom     <- ruleCases [liftA1 Named         (tokenConstructor @"Name"),
                           liftA1 NumberLiteral (tokenConstructor @"Number"),
                           liftA1 TextLiteral   (tokenConstructor @"Text"),
                           liftA2 Call          (tokenConstructor @"Name") (bracketed T.Round (separatedBy [T.Comma] expression)),
                           bracketed T.Round expression]

    unary    <- ruleCases [liftA2 UnaryOperator (tokenConstructor @"UnaryOperator") atom,
                           atom]

    binaries <- ruleCases [liftA3 BinaryOperation  unary (tokenConstructor @"BinaryOperator") binaries,
                           liftA1 SingleExpression unary]

    let expression = liftA1 resolvePrecedences binaries

    return expression

blockGrammar :: Grammar r (Block Text)
blockGrammar = mdo
    expression <- expressionGrammar
    -----------------------------------------------------------
    binding <- E.rule $ do
        letvar <- E.terminal (\case T.Keyword T.K_let -> Just Let; T.Keyword T.K_var -> Just Var; _ -> Nothing) -- TODO prism?
        name   <- tokenConstructor @"Name"
        token T.EqualsSign
        rhs    <- expression
        token T.Semicolon
        return (Binding letvar name rhs)
    assign <- E.rule $ do
        lhs <- tokenConstructor @"Name"
        token T.EqualsSign
        rhs <- expression
        token T.Semicolon
        return (Assign lhs rhs)
    ifthen <- E.rule $ do
        keyword T.K_if
        cond <- expression
        body <- block
        return (IfThen cond body)
    ifthenelse <- E.rule $ do
        keyword T.K_if
        cond  <- expression
        body1 <- block
        keyword T.K_else
        body2 <- block
        return (IfThenElse cond body1 body2)
    forever <- E.rule $ do
        keyword T.K_forever
        body <- block
        return (Forever body)
    while <- E.rule $ do
        keyword T.K_while
        cond <- expression
        body <- block
        return (While cond body)
    ret <- E.rule $ do
        keyword T.K_return
        arg <- liftA1 head (zeroOrOne expression)
        token T.Semicolon
        return (Return arg)
    break <- E.rule $ do
        keyword T.K_break
        token T.Semicolon
        return Break
    exprStatement <- E.rule $ do
        expr <- expression
        token T.Semicolon
        return (Expression expr)
    -----------------------------------------------------
    statement <- E.rule (oneOf [binding, assign, ifthen, ifthenelse, forever, while, ret, break, exprStatement])
    block     <- E.rule (liftA1 Block (bracketed T.Curly (oneOrMore statement)))
    return block

functionGrammar :: Grammar r (Function Text)
functionGrammar = do
    block <- blockGrammar
    argument <- E.rule $ do
        argumentName <- tokenConstructor @"Name"
        token T.Colon
        argumentType <- tokenConstructor @"Name"
        return Argument { argumentName, argumentType }
    E.rule $ do
        keyword T.K_function
        functionName <- tokenConstructor @"Name"
        arguments    <- bracketed T.Round (separatedBy [T.Comma] argument)
        returns      <- liftA1 head (zeroOrOne (keyword T.K_returns `followedBy` tokenConstructor @"Name"))
        body         <- block
        return Function { functionName, arguments, returns, body }


type AST name = [Function name]

data Error
    = Invalid   !Int ![Expected] ![T.Token]
    | Ambiguous ![AST Text]
    deriving (Generic, Show)

parse :: [T.Token] -> Either Error (AST Text)
parse tokens = case E.fullParses (E.parser (liftM oneOrMore functionGrammar)) tokens of
    ([], E.Report a b c) -> Left (Invalid a b c)
    ([one], _) -> Right one
    (more,  _) -> Left (Ambiguous more)





----------------------------------------------------------------------------- pretty-printing

renderBlock :: RenderName name => Block name -> P.Document
renderBlock block = P.braces (P.nest 4 (P.hardline ++ P.render block) ++ P.hardline)

class RenderName name where
    renderName :: P.DefinitionOrUse -> name -> P.Document

instance RenderName Text where
    renderName defOrUse name = P.note (P.Identifier (P.IdentInfo name defOrUse P.UnresolvedName Nothing)) (P.pretty name)

instance RenderName name => P.Render (Expression name) where
    render = \case
        Named          name           -> renderName P.Use name
        Call           name args      -> renderName P.Use name ++ P.parens (P.hsep (P.punctuate "," (map P.render args)))
        NumberLiteral  number         -> P.number number
        TextLiteral    text           -> P.string text
        UnaryOperator  op expr        -> P.unaryOperator op ++ P.render expr
        BinaryOperator expr1 op expr2 -> P.render expr1 ++ " " ++ P.binaryOperator op ++ " " ++ P.render expr2

instance RenderName name => P.Render (Statement name) where
    render = \case
        Binding    btype name expr    -> P.keyword (case btype of Let -> "let"; Var -> "var") ++ " " ++ renderName P.Definition name ++ " " ++ P.defineEquals ++ " " ++ P.render expr ++ P.semicolon
        Assign     name expr          -> renderName P.Use name ++ " " ++ P.assignEquals ++ " " ++ P.render expr ++ P.semicolon
        IfThen     expr block         -> P.keyword "if" ++ " " ++ P.render expr ++ " " ++ renderBlock block
        IfThenElse expr block1 block2 -> P.render (IfThen expr block1) ++ " " ++ P.keyword "else" ++ " " ++ renderBlock block2
        Forever    block              -> P.keyword "forever" ++ " " ++ renderBlock block
        While      expr block         -> P.keyword "while" ++ " " ++ P.render expr ++ " " ++ renderBlock block
        Return     maybeExpr          -> P.keyword "return" ++ (maybe "" (\expr -> " " ++ P.render expr) maybeExpr) ++ P.semicolon
        Break                         -> P.keyword "break" ++ P.semicolon
        Expression expr               -> P.render expr ++ P.semicolon

instance RenderName name => P.Render (Block name) where
    render (Block statements) = mconcat (P.punctuate P.hardline (map P.render statements))

instance RenderName name => P.Render (Argument name) where
    render Argument { argumentName, argumentType } = renderName P.Definition argumentName ++ P.colon ++ " " ++ renderName P.Use argumentType

instance RenderName name => P.Render (Function name) where
    render Function { functionName, arguments, returns, body } = renderedHead ++ renderedArguments ++ renderedReturns ++ renderedBody where
        renderedHead      = P.keyword "function" ++ " " ++ renderName P.Definition functionName
        renderedArguments = P.parens (P.hsep (P.punctuate "," (map P.render arguments)))
        renderedReturns   = maybe "" (\returnType -> " " ++ P.keyword "returns" ++ " " ++ renderName P.Use returnType) returns
        renderedBody      = P.hardline ++ renderBlock body

instance RenderName name => P.Render (AST name) where
    render = mconcat . P.punctuate P.hardline . map P.render
