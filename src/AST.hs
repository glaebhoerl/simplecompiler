module AST where

import qualified Text.Earley as E

import MyPrelude
import qualified Token as T

type Name = Text

data Expression
    = Name           !Name
    | Literal        !Integer
    | UnaryOperator  !UnaryOperator  !Expression
    | BinaryOperator !BinaryOperator !Expression !Expression
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
                        bracketed T.Round logicals]

    unary <- ruleCases [liftA2 UnaryOperator (E.terminal (\case T.UnaryOperator op -> Just op; _ -> Nothing)) atom,
                        atom]

    mulDivMod  <- ruleCases [liftA3 BinaryOperator (E.terminal (\case T.BinaryOperator binop@(ArithmeticOperator op) | op `elem` [Mul, Div, Mod] -> Just binop; _ -> Nothing)) mulDivMod unary,
                            unary]

    arithmetic <- ruleCases [liftA3 BinaryOperator (E.terminal (\case T.BinaryOperator binop@(ArithmeticOperator op) | op `elem` [Add, Sub]      -> Just binop; _ -> Nothing)) arithmetic mulDivMod,
                                mulDivMod]

    comparisons <- ruleCases [liftA3 BinaryOperator (E.terminal (\case T.BinaryOperator binop@ComparisonOperator{} -> Just binop; _ -> Nothing)) comparisons arithmetic,
                                arithmetic]

    logicals <- ruleCases [liftA3 BinaryOperator (E.terminal (\case T.BinaryOperator binop@LogicalOperator{} -> Just binop; _ -> Nothing)) logicals comparisons,
                            comparisons]
    return logicals

statementGrammar :: Grammar r Statement
statementGrammar = todo

parse :: [T.Token] -> [Statement]
parse tokens = todo --head (fst (E.fullParses (E.parser statementGrammar) tokens)) -- FIXME
