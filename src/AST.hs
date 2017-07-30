module AST where

import qualified Text.Earley as E

import MyPrelude
import qualified Token as T

type Name = Text

data Expression
    = Name           !Name
    | Literal        !Integer
    | UnaryOperator  !UnaryOperator !Expression
    | BinaryOperator !Expression    !BinaryOperator  !Expression
    | Read
    deriving (Eq, Show)

data Statement
    = Let        !Name        !Expression
    | Var        !Name        !Expression
    | Assign     !Name        !Expression
    | IfThen     !Expression  ![Statement]
    | IfThenElse !Expression  ![Statement] ![Statement]
    | Forever    ![Statement]
    | While      !Expression  ![Statement]
    | Return     !Expression
    | Break
    | Write      !Expression
    deriving (Eq, Show)

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

expressionGrammar :: Grammar r Expression
expressionGrammar = mdo
    atom <- ruleCases [liftA1 Name    (E.terminal (\case T.Name   n -> Just n; _ -> Nothing)),
                       liftA1 Literal (E.terminal (\case T.Number n -> Just n; _ -> Nothing)),
                       liftA1 (const Read) (E.list [T.Name "read", T.Bracket' (T.Bracket T.Round T.Open), T.Bracket' (T.Bracket T.Round T.Close)]),
                       bracketed T.Round logicals]

    unary <- ruleCases [liftA2 UnaryOperator (E.terminal (\case T.UnaryOperator op -> Just op; _ -> Nothing)) atom,
                        atom]

    mulDivMod  <- ruleCases [liftA3 BinaryOperator mulDivMod (E.terminal (\case T.BinaryOperator binop@(ArithmeticOperator op) | op `elem` [Mul, Div, Mod] -> Just binop; _ -> Nothing)) unary,
                            unary]

    arithmetic <- ruleCases [liftA3 BinaryOperator arithmetic (E.terminal (\case T.BinaryOperator binop@(ArithmeticOperator op) | op `elem` [Add, Sub]      -> Just binop; _ -> Nothing)) mulDivMod,
                             mulDivMod]

    comparisons <- ruleCases [liftA3 BinaryOperator comparisons (E.terminal (\case T.BinaryOperator binop@ComparisonOperator{} -> Just binop; _ -> Nothing)) arithmetic,
                              arithmetic]

    logicals <- ruleCases [liftA3 BinaryOperator logicals (E.terminal (\case T.BinaryOperator binop@LogicalOperator{} -> Just binop; _ -> Nothing)) comparisons,
                           comparisons]
    return logicals

statementGrammar :: Grammar r Statement
statementGrammar = todo

data Error = Invalid Int [Expected] [T.Token] | Ambiguous [Expression] deriving Show

parse :: [T.Token] -> Either Error Expression
parse tokens = case E.fullParses (E.parser expressionGrammar) tokens of
    ([], E.Report a b c) -> Left (Invalid a b c)
    ([one], _) -> Right one
    (more,  _) -> Left (Ambiguous more)
