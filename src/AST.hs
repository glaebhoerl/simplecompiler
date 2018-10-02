module AST (Expression (..), BindingType (..), Statement (..), Block (..), AST, Error (..), parse, RenderName (..)) where

import MyPrelude

import qualified Text.Earley as E

import qualified Pretty as P
import qualified Token  as T

data Expression name
    = Named          !name
    | NumberLiteral  !Integer
    | TextLiteral    !Text
    | UnaryOperator  !UnaryOperator     !(Expression name)
    | BinaryOperator !(Expression name) !BinaryOperator !(Expression name)
    | Ask            !(Expression name)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data BindingType
    = Let
    | Var
    deriving (Generic, Eq, Show)

-- TODO: add "plain blocks"
data Statement name
    = Binding    !BindingType       !name              !(Expression name)
    | Assign     !name              !(Expression name)
    | IfThen     !(Expression name) !(Block name)
    | IfThenElse !(Expression name) !(Block name)      !(Block name)
    | Forever    !(Block name)
    | While      !(Expression name) !(Block name)
    | Return     !(Maybe (Expression name))
    | Break
    | Say        !(Expression name)
    | Write      !(Expression name)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

newtype Block name = Block {
    body :: [Statement name]
} deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

type Expected = Text

type Prod    r output = E.Prod r Expected T.Token output
type Grammar r output = E.Grammar r (Prod r output)

bracketed :: T.BracketKind -> Prod r output -> Prod r output
bracketed kind inner = do
    _      <- E.token (T.Bracket' (T.Bracket kind T.Open))
    output <- inner
    _      <- E.token (T.Bracket' (T.Bracket kind T.Close))
    return output

ruleCases :: [Prod r output] -> Grammar r output
ruleCases cases = E.rule (oneOf cases)

followedBy :: Applicative f => f a -> f b -> f b
followedBy = (*>)

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
    = Expression (Expression Text)
    | BinaryOperation (Expression Text) BinaryOperator BinaryOperationList
    deriving Show

resolvePrecedences :: BinaryOperationList -> Expression Text
resolvePrecedences binaryOperationList = finalResult where
    finalResult = case allPrecedencesResolved of
        Expression expr -> expr
        list            -> bug ("Unresolved binary operator precedence: " ++ prettyShow list)
    allPrecedencesResolved = foldl' resolveOnePrecedenceLevel binaryOperationList precedenceGroups
    resolveOnePrecedenceLevel binOpList precedenceGroup = case binOpList of
        BinaryOperation expr1 op1 (BinaryOperation expr2 op2 rest)
            | elem op1 precedenceGroup -> resolveOnePrecedenceLevel (BinaryOperation (BinaryOperator expr1 op1 expr2) op2 rest) precedenceGroup
            | otherwise                -> BinaryOperation expr1 op1 (resolveOnePrecedenceLevel (BinaryOperation expr2 op2 rest) precedenceGroup)
        BinaryOperation expr1 op (Expression expr2)
            | elem op  precedenceGroup -> Expression (BinaryOperator expr1 op expr2)
        other -> other

expressionGrammar :: Grammar r (Expression Text)
expressionGrammar = mdo
    atom     <- ruleCases [liftA1 Named         (E.terminal (getWhen (constructor @"Name"))),
                           liftA1 NumberLiteral (E.terminal (getWhen (constructor @"Number"))),
                           liftA1 TextLiteral   (E.terminal (getWhen (constructor @"Text"))),
                           liftA1 Ask           (E.token (T.Name "ask") `followedBy` bracketed T.Round expression),
                           bracketed T.Round expression]

    unary    <- ruleCases [liftA2 UnaryOperator (E.terminal (getWhen (constructor @"UnaryOperator"))) atom,
                           atom]

    binaries <- ruleCases [liftA3 BinaryOperation unary (E.terminal (getWhen (constructor @"BinaryOperator"))) binaries,
                           liftA1 Expression unary]

    let expression = fmap resolvePrecedences binaries

    return expression

blockGrammar :: Grammar r (Block Text)
blockGrammar = mdo
    expression <- expressionGrammar
    let keyword kw = E.token (T.Keyword kw)
    -----------------------------------------------------------
    binding <- E.rule $ do
        letvar <- E.terminal (\case T.Keyword T.K_let -> Just Let; T.Keyword T.K_var -> Just Var; _ -> Nothing) -- TODO prism?
        name   <- E.terminal (getWhen (constructor @"Name"))
        _      <- E.token T.EqualsSign
        rhs    <- expression
        _      <- E.token T.Semicolon
        return (Binding letvar name rhs)
    assign <- E.rule $ do
        lhs <- E.terminal (getWhen (constructor @"Name"))
        _   <- E.token T.EqualsSign
        rhs <- expression
        _   <- E.token T.Semicolon
        return (Assign lhs rhs)
    ifthen <- E.rule $ do
        _    <- keyword T.K_if
        cond <- expression
        body <- block
        return (IfThen cond body)
    ifthenelse <- E.rule $ do
        _     <- keyword T.K_if
        cond  <- expression
        body1 <- block
        _     <- keyword T.K_else
        body2 <- block
        return (IfThenElse cond body1 body2)
    forever <- E.rule $ do
        _    <- keyword T.K_forever
        body <- block
        return (Forever body)
    while <- E.rule $ do
        _    <- keyword T.K_while
        cond <- expression
        body <- block
        return (While cond body)
    ret <- E.rule $ do
        _   <- keyword T.K_return
        arg <- liftA1 head (zeroOrOne expression)
        _   <- E.token T.Semicolon
        return (Return arg)
    break <- E.rule $ do
        _ <- keyword T.K_break
        _ <- E.token T.Semicolon
        return Break
    say <- E.rule $ do
        _    <- E.token (T.Name "say")
        expr <- bracketed T.Round expression
        _    <- E.token T.Semicolon
        return (Say expr)
    write <- E.rule $ do
        _    <- E.token (T.Name "write")
        expr <- bracketed T.Round expression
        _    <- E.token T.Semicolon
        return (Write expr)
    -----------------------------------------------------
    oneStatement <- E.rule $ oneOf [binding, assign, ifthen, ifthenelse, forever, while, ret, break, say, write]
    moreStatements <- E.rule $ do
        first <- oneStatement
        --_     <- oneOrMore (E.token T.Semicolon)
        rest  <- oneOrMoreStatements
        return (first : rest)
    oneOrMoreStatements <- E.rule (oneOf [liftA1 single oneStatement, moreStatements])
    -- FIXME: this doesn't accept empty blocks but rn idgaf
    statements <- E.rule $ do
        s <- oneOrMoreStatements
        return s
    block <- E.rule (liftA1 Block (bracketed T.Curly statements))
    return (liftA1 Block statements)

type AST = Block

data Error
    = Invalid   !Int ![Expected] ![T.Token]
    | Ambiguous ![AST Text]
    deriving (Generic, Show)

parse :: [T.Token] -> Either Error (AST Text)
parse tokens = case E.fullParses (E.parser blockGrammar) tokens of
    ([], E.Report a b c) -> Left (Invalid a b c)
    ([one], _) -> Right one
    (more,  _) -> Left (Ambiguous more)

nameText :: P.IdentSort -> P.DefinitionOrUse -> Text -> P.Document
nameText defOrUse sort name = P.note (P.Identifier (P.IdentInfo name sort defOrUse Nothing)) (P.pretty name)

builtinName :: Text -> P.Document
builtinName = nameText P.BuiltinName P.Use

renderBlock :: RenderName name => Block name -> P.Document
renderBlock block = P.braces (P.nest 4 (P.hardline ++ P.render block) ++ P.hardline)

class RenderName name where
    renderName :: name -> P.DefinitionOrUse -> P.Document

instance RenderName Text where
    renderName = flip (nameText P.UnresolvedName)

instance RenderName name => P.Render (Expression name) where
    render = \case
        Named          name           -> renderName name P.Use
        NumberLiteral  number         -> P.number number
        TextLiteral    text           -> P.string text
        UnaryOperator  op expr        -> P.unaryOperator op ++ P.render expr
        BinaryOperator expr1 op expr2 -> P.render expr1 ++ " " ++ P.binaryOperator op ++ " " ++ P.render expr2
        Ask            expr           -> builtinName "ask" ++ P.parens (P.render expr)

instance RenderName name => P.Render (Statement name) where
    render = \case
        Binding    btype name expr    -> P.keyword (case btype of Let -> "let"; Var -> "var") ++ " " ++ renderName name P.Definition ++ " " ++ P.defineEquals ++ " " ++ P.render expr ++ P.semicolon
        Assign     name expr          -> renderName name P.Use ++ " " ++ P.assignEquals ++ " " ++ P.render expr ++ P.semicolon
        IfThen     expr block         -> P.keyword "if" ++ " " ++ P.render expr ++ " " ++ renderBlock block
        IfThenElse expr block1 block2 -> P.render (IfThen expr block1) ++ " " ++ P.keyword "else" ++ " " ++ renderBlock block2
        Forever    block              -> P.keyword "forever" ++ " " ++ renderBlock block
        While      expr block         -> P.keyword "while" ++ " " ++ P.render expr ++ " " ++ renderBlock block
        Return     maybeExpr          -> P.keyword "return" ++ (maybe "" (\expr -> " " ++ P.render expr) maybeExpr) ++ P.semicolon
        Break                         -> P.keyword "break" ++ P.semicolon
        Say        expr               -> builtinName "say"   ++ P.parens (P.render expr) ++ P.semicolon
        Write      expr               -> builtinName "write" ++ P.parens (P.render expr) ++ P.semicolon

instance RenderName name => P.Render (Block name) where
    render (Block statements) = mconcat (P.punctuate P.hardline (map P.render statements))
