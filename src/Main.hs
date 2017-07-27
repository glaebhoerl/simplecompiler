module Main where

import MyPrelude

import Data.Text (Text)


{-
FIRST
  two types: Int, Bool
  arithmetic and comparison operators
  `if`. `while`? `forever`/`break`? as expressions?
  `let`
  `read()`, `write()`
  mutation (assignment)? type annotations? strings?
  no functions yet

Example program:
    var n = 0
    forever {
        write(n)
        let m = read()
        if m == 0 {
            return
        }
        n = n + m
    }
-}

data Keyword = K_break | K_let | K_forever | K_if | K_return | K_var | K_while

data ArithmeticOperator = Add | Sub | Mul | Div | Mod

data ComparisonOperator = Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual

data LogicalOperator = And | Or

data BinaryOperator = ArithmeticOperator !ArithmeticOperator | ComparisonOperator !ComparisonOperator | LogicalOperator !LogicalOperator

data UnaryOperator = Not | Negate

data BracketKind = Round | Curly | Square

data BracketDirection = Open | Close

data Bracket = Bracket { bracketKind :: !BracketKind, bracketDirection :: !BracketDirection }

data Token = TKeyword !Keyword | TName !Text | TBinaryOperator !BinaryOperator | TUnaryOperator !UnaryOperator | TBracket !Bracket | TNumber !Integer | EqualsSign | Comma | Semicolon | Newline

type Name = Text

data Expression = Name' !Name
                | Literal !Integer
                | UnaryOperator !UnaryOperator !Expression
                | BinaryOperator !BinaryOperator !Expression !Expression

data Statement = Let !Name !Expression
               | Var !Name !Expression
               | Assign !Name !Expression
               | IfThen !Expression ![Statement]
               | IfThenElse !Expression ![Statement] ![Statement]
               | Forever ![Statement]
               | While !Expression ![Statement]
               | Return !Expression
               | Break
               | Write !Expression


tokenize :: Text -> [Token]
tokenize = todo

parse :: [Token] -> Syntax
parse = todo

alsoParse :: Syntax -> [Statement]
alsoParse = todo

main :: IO ()
main = do
  putStrLn "hello world"
