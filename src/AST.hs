module AST (Expression (..), BindingType (..), Statement (..), Block (..), AST, Error (..), parse) where

import MyPrelude

import qualified Text.Earley as E

import qualified Token as T

data Expression name
    = Name           !name
    | Literal        !Integer
    | UnaryOperator  !UnaryOperator     !(Expression name)
    | BinaryOperator !(Expression name) !BinaryOperator !(Expression name)
    | Ask            !Text
    deriving (Eq, Show, Functor, Foldable, Traversable)

data BindingType
    = Let
    | Var
    deriving (Eq, Show)

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
    | Say        !Text
    | Write      !(Expression name)
    deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Block name = Block {
    body :: [Statement name]
} deriving (Eq, Show, Functor, Foldable, Traversable)

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

eatNewlines :: Prod r ()
eatNewlines = liftA1 (const ()) (zeroOrMore (E.token T.Newline))

-- TODO: I should specify the precedence levels declaratively somehow and figure out how to generate the grammar from that
-- TODO: eat newlines here too
expressionGrammar :: Grammar r (Expression Text)
expressionGrammar = mdo
    atom        <- ruleCases [liftA1 Name
                                     (E.terminal (\case T.Name   n -> Just n; _ -> Nothing)),
                              liftA1 Literal
                                     (E.terminal (\case T.Number n -> Just n; _ -> Nothing)),
                              liftA1 Ask
                                     (E.token (T.Name "ask") `followedBy` bracketed T.Round (E.terminal (\case T.Text text -> Just text; _ -> Nothing))),
                              bracketed T.Round logicals]

    unary       <- ruleCases [liftA2 UnaryOperator
                                     (E.terminal (\case T.UnaryOperator op -> Just op; _ -> Nothing))
                                     atom,
                              atom]

    mulDivMod   <- ruleCases [liftA3 BinaryOperator
                                     mulDivMod
                                     (E.terminal (\case T.BinaryOperator binop@(ArithmeticOperator op) | op `elem` [Mul, Div, Mod] -> Just binop; _ -> Nothing))
                                     unary,
                              unary]

    arithmetic  <- ruleCases [liftA3 BinaryOperator
                                     arithmetic
                                     (E.terminal (\case T.BinaryOperator binop@(ArithmeticOperator op) | op `elem` [Add, Sub] -> Just binop; _ -> Nothing))
                                     mulDivMod,
                              mulDivMod]

    comparisons <- ruleCases [liftA3 BinaryOperator
                                     comparisons
                                     (E.terminal (\case T.BinaryOperator binop@ComparisonOperator{} -> Just binop; _ -> Nothing))
                                     arithmetic,
                              arithmetic]

    -- FIXME we should give && higher precedence than ||, or require parentheses to disambiguate
    logicals    <- ruleCases [liftA3 BinaryOperator
                                     logicals
                                     (E.terminal (\case T.BinaryOperator binop@LogicalOperator{} -> Just binop; _ -> Nothing))
                                     comparisons,
                              comparisons]
    return logicals

-- FIXME: this newline eating is ugly as fuck, how can I do something better?
blockGrammar :: Grammar r (Block Text)
blockGrammar = mdo
    expression <- expressionGrammar
    let keyword kw = E.token (T.Keyword kw)
    -----------------------------------------------------------
    binding <- E.rule $ do
        letvar <- E.terminal (\case T.Keyword T.K_let -> Just Let; T.Keyword T.K_var -> Just Var; _ -> Nothing)
        _      <- eatNewlines
        name   <- E.terminal (\case T.Name n -> Just n; _ -> Nothing)
        _      <- eatNewlines
        _      <- E.token T.EqualsSign
        _      <- eatNewlines
        rhs    <- expression
        return (Binding letvar name rhs)
    assign <- E.rule $ do
        lhs <- E.terminal (\case T.Name n -> Just n; _ -> Nothing)
        _   <- eatNewlines
        _   <- E.token T.EqualsSign
        _   <- eatNewlines
        rhs <- expression
        return (Assign lhs rhs)
    ifthen <- E.rule $ do
        _    <- keyword T.K_if
        _    <- eatNewlines
        cond <- expression
        _    <- eatNewlines
        body <- block
        return (IfThen cond body)
    ifthenelse <- E.rule $ do
        (IfThen cond body1) <- ifthen -- HACK
        _     <- eatNewlines
        _     <- keyword T.K_else
        _     <- eatNewlines
        body2 <- block
        return (IfThenElse cond body1 body2)
    forever <- E.rule $ do
        _    <- keyword T.K_forever
        _    <- eatNewlines
        body <- block
        return (Forever body)
    while <- E.rule $ do
        _    <- keyword T.K_while
        _    <- eatNewlines
        cond <- expression
        _    <- eatNewlines
        body <- block
        return (While cond body)
    ret <- E.rule $ do
        _   <- keyword T.K_return
        arg <- liftA1 head (zeroOrOne (eatNewlines `followedBy` expression))
        return (Return arg)
    break <- E.rule $ do
        _ <- keyword T.K_break
        return Break
    say   <- E.rule (liftA1 Say   (E.token (T.Name "say")   `followedBy` bracketed T.Round (E.terminal (\case T.Text text -> Just text; _ -> Nothing))))
    write <- E.rule (liftA1 Write (E.token (T.Name "write") `followedBy` bracketed T.Round expression))
    -----------------------------------------------------
    oneStatement <- E.rule $ oneOf [binding, assign, ifthen, ifthenelse, forever, while, ret, break, say, write]
    moreStatements <- E.rule $ do
        first <- oneStatement
        _     <- oneOrMore (oneOf [E.token T.Semicolon, E.token T.Newline])
        rest  <- oneOrMoreStatements
        return (first : rest)
    oneOrMoreStatements <- E.rule (oneOf [liftA1 single oneStatement, moreStatements])
    -- FIXME: this doesn't accept empty blocks but rn idgaf
    statements <- E.rule $ do
        _ <- eatNewlines
        s <- oneOrMoreStatements
        _ <- eatNewlines
        return s
    block <- E.rule (liftA1 Block (bracketed T.Curly statements))
    return (liftA1 Block statements)

type AST = Block

data Error
    = Invalid   !Int ![Expected] ![T.Token]
    | Ambiguous ![AST Text]
    deriving Show

parse :: [T.Token] -> Either Error (AST Text)
parse tokens = case E.fullParses (E.parser blockGrammar) tokens of
    ([], E.Report a b c) -> Left (Invalid a b c)
    ([one], _) -> Right one
    (more,  _) -> Left (Ambiguous more)
