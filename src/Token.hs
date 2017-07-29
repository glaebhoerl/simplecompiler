module Token where

import Data.Char (isAlpha, isAlphaNum, isDigit)
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
        BinaryOperator binop   -> toText binop
        UnaryOperator  unop    -> toText unop
        Bracket'       bracket -> toText bracket
        Name           name    -> toText name
        Number         number  -> toText number
        EqualsSign             -> "="
        Comma                  -> ","
        Semicolon              -> ";"
        Newline                -> "\n"

type Expected = Text

type Prod    r output = E.Prod r Expected Char output
type Grammar r output = E.Grammar r (Prod r output)

matchRepresentable :: TextRepresentation a => a -> Prod r a
matchRepresentable a = do
    _ <- E.listLike (toText a)
    return a

match :: (Enumerable a, TextRepresentation a) => (a -> Token) -> Prod r Token
match toToken = liftA1 toToken (oneOf (map matchRepresentable enumerate))

whitespace :: Prod r Char
whitespace = oneOf (map E.token [' ', '\n'])

whitespaced :: Prod r output -> Prod r output
whitespaced inner = do
    _      <- whitespace
    output <- inner
    _      <- whitespace
    return output

orUnderscore :: (Char -> Bool) -> (Char -> Bool)
orUnderscore f = \c -> f c || (c == '_')

literal :: Token -> Prod r Token
literal token = do
    _ <- E.listLike (toText token)
    return token

name :: Prod r Text
name = do
    first <- (E.satisfy (orUnderscore isAlpha))
    rest  <- (zeroOrMore (E.satisfy (orUnderscore isAlphaNum)))
    return (Text.pack (first : rest))

number :: Prod r Integer
number = do
    minusSign <- zeroOrOne (E.token '-')
    digits    <- oneOrMore (E.satisfy isDigit)
    return (read (minusSign ++ digits))

token :: Prod r Token
token = oneOf [
            match Keyword,
            whitespaced (match BinaryOperator),
            match UnaryOperator,
            match Bracket',
            liftA1 Name name,
            liftA1 Number number,
            whitespaced (literal EqualsSign),
            literal Comma,
            literal Semicolon,
            literal Newline
        ]

tokens :: Prod r [Token]
tokens = do
    _    <- zeroOrMore whitespace
    toks <- zeroOrMore $ do
        tok <- token
        _   <- zeroOrMore whitespace
        return tok
    return toks

tokenize :: Text -> Maybe [Token]
tokenize text = case fst (E.fullParses (E.parser (E.rule tokens)) text) of
    (parse:_) -> Just parse
    []        -> Nothing
