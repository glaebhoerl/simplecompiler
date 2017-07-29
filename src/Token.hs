module Token where

import qualified Data.Text   as Text
import qualified Text.Earley as E

import MyPrelude

class TextRepresentation a where
    toText :: a -> Text

instance TextRepresentation Text where
    toText = id

instance TextRepresentation Integer where
    toText = Text.pack . show

instance TextRepresentation ArithmeticOperator where
    toText = \case
        Add -> "+"
        Sub -> "-"
        Mul -> "*"
        Div -> "/"
        Mod -> "%"

instance TextRepresentation ComparisonOperator where
    toText = \case
        Less         -> "<"
        LessEqual    -> "<="
        Greater      -> ">"
        GreaterEqual -> ">="
        Equal        -> "=="
        NotEqual     -> "!="

instance TextRepresentation LogicalOperator where
    toText = \case
        And -> "&&"
        Or  -> "||"

instance TextRepresentation BinaryOperator where
    toText = \case
        ArithmeticOperator op -> toText op
        ComparisonOperator op -> toText op
        LogicalOperator    op -> toText op

instance TextRepresentation UnaryOperator where
    toText = \case
        Not    -> "!"
        Negate -> "-"

data Keyword
    = K_break
    | K_let
    | K_forever
    | K_if
    | K_return
    | K_var
    | K_while
    deriving (Eq, Show, Enum, Bounded, Enumerable)

instance TextRepresentation Keyword where
    toText = Text.pack . drop 2 . show

data BracketKind
    = Round
    | Curly
    | Square
    deriving (Eq, Show, Enum, Bounded, Enumerable)

data BracketDirection
    = Open
    | Close
    deriving (Eq, Show, Enum, Bounded, Enumerable)

data Bracket = Bracket {
    bracketKind      :: !BracketKind,
    bracketDirection :: !BracketDirection
} deriving (Eq, Show)

instance Enumerable Bracket where
    enumerate = [Bracket kind dir | kind <- enumerate, dir <- enumerate]

instance TextRepresentation Bracket where
    toText = \case
        Bracket Round  Open  -> "("
        Bracket Round  Close -> ")"
        Bracket Curly  Open  -> "{"
        Bracket Curly  Close -> "}"
        Bracket Square Open  -> "["
        Bracket Square Close -> "]"

data Token
    = Keyword        !Keyword
    | Name           !Text
    | BinaryOperator !BinaryOperator
    | UnaryOperator  !UnaryOperator
    | Bracket'       !Bracket
    | Number         !Integer
    | EqualsSign
    | Comma
    | Semicolon
    | Newline
    deriving (Eq, Show)

instance TextRepresentation Token where
    toText = \case
        Keyword        keyword -> toText keyword
        Name           name    -> toText name
        BinaryOperator op      -> toText op
        UnaryOperator  op      -> toText op
        Bracket'       bracket -> toText bracket
        Number         number  -> toText number
        EqualsSign             -> "="
        Comma                  -> ","
        Semicolon              -> ";"
        Newline                -> "\n"

type Expected = Text

type Prod    r = E.Prod r Expected Char [Token]
type Grammar r = E.Grammar r (Prod r)

tokensGrammar :: Grammar r
tokensGrammar = do
    keyword <- E.rule (E.satisfy (\word -> elem word (map (drop 2 . show) (enumerate @Keyword))))

    return todo

tokenize :: Text -> [Token]
tokenize = todo
