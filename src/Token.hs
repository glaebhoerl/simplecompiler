module Token (Token (..), Keyword (..), Bracket (..), BracketKind (..), BracketDirection (..), Error (..), tokenize) where

import MyPrelude

import Data.Char (isAlpha, isAlphaNum)

import qualified Text.Regex.Applicative        as RE
import qualified Text.Regex.Applicative.Common as RE (signed, decimal)
import qualified Language.Lexer.Applicative    as Lex
import qualified Data.Loc                      as Loc

import qualified Pretty as P

class TextRepresentation a where
    toText :: a -> Text

instance TextRepresentation Text where
    toText = id

instance TextRepresentation Integer where
    toText = showText

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
    | K_else
    | K_forever
    | K_function
    | K_if
    | K_let
    | K_return
    | K_returns
    | K_var
    | K_while
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable Keyword

instance TextRepresentation Keyword where
    toText = stringToText . drop 2 . show

data BracketKind
    = Round
    | Curly
    | Square
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable BracketKind

data BracketDirection
    = Open
    | Close
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable BracketDirection

data Bracket = Bracket {
    bracketKind      :: BracketKind,
    bracketDirection :: BracketDirection
} deriving (Generic, Eq, Show)

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
    = Keyword        Keyword
    | Name           Text
    | BinaryOperator BinaryOperator
    | UnaryOperator  UnaryOperator
    | Bracket'       Bracket
    | Number         Integer
    | Text           Text
    | EqualsSign
    | Comma
    | Colon
    | Semicolon
    deriving (Generic, Eq, Show)

instance TextRepresentation Token where
    toText = \case
        Keyword        keyword -> toText keyword
        BinaryOperator binop   -> toText binop
        UnaryOperator  unop    -> toText unop
        Bracket'       bracket -> toText bracket
        Name           name'   -> toText name'
        Number         number' -> toText number'
        Text           text'   -> toText text'
        EqualsSign             -> "="
        Comma                  -> ","
        Colon                  -> ":"
        Semicolon              -> ";"

instance P.Render Token where
    render = \case
        Keyword        keyword -> P.keyword (toText keyword)
        BinaryOperator binop   -> P.binaryOperator binop
        UnaryOperator  unop    -> P.unaryOperator unop
        Name           name'   -> P.note (P.Identifier (P.IdentInfo name' P.Use P.Unknown False)) (P.pretty name')
        Number         number' -> P.number number'
        Text           text'   -> P.string text'
        EqualsSign             -> P.defineEquals
        Comma                  -> ","
        Colon                  -> P.colon
        Semicolon              -> P.semicolon
        Bracket'       bracket -> P.note (bracketInfo (bracketKind bracket)) (P.pretty (toText bracket)) where
            bracketInfo = \case
                Round  -> P.Paren
                Curly  -> P.Brace
                Square -> P.Bracket

instance P.Render [Token] where
    render = P.hsep . map P.render

instance P.Render [With Loc Token] where -- sighhh
    render = P.hsep . map P.render

type RE = RE.RE Char

matchRepresentable :: TextRepresentation a => a -> RE a
matchRepresentable a = do
    (unused . RE.string . textToString . toText) a
    return a

matchEnumerable :: (Enumerable a, TextRepresentation a) => (a -> Token) -> RE Token
matchEnumerable toToken = liftA1 toToken (oneOf (map matchRepresentable enumerate))

literal :: Token -> RE Token
literal = matchRepresentable

orUnderscore :: (Char -> Bool) -> (Char -> Bool)
orUnderscore f = \c -> f c || (c == '_')

name :: RE Text
name = do
    first <- RE.psym (orUnderscore isAlpha)
    rest  <- zeroOrMore (RE.psym (orUnderscore isAlphaNum))
    return (stringToText (first : rest))

nameOrKeyword :: Text -> Token
nameOrKeyword text' = case lookup text' (map (\keyword -> (toText keyword, keyword)) (enumerate @Keyword)) of
    Just keyword -> Keyword keyword
    Nothing      -> Name text'

number :: RE Integer
number = RE.signed RE.decimal

text :: RE Text
text = do
    unused (RE.sym '"')
    content <- zeroOrMore (RE.psym (\c -> notElem c ['"', '\n']))
    unused (RE.sym '"')
    return (stringToText content)

token :: RE Token
token = oneOf
    [liftA1 nameOrKeyword name,
     liftA1 Number        number,
     liftA1 Text          text,
     matchEnumerable UnaryOperator,
     matchEnumerable BinaryOperator,
     matchEnumerable Bracket',
     literal EqualsSign,
     literal Comma,
     literal Colon,
     literal Semicolon]

whitespace :: RE Char
whitespace = oneOf (map RE.sym [' ', '\n'])

tokens :: Lex.Lexer Token
tokens = Lex.token (Lex.longest token) ++ Lex.whitespace (Lex.longest whitespace)

data Error = InvalidTokenAt Loc.Pos deriving (Generic, Show)

tokenize :: Text -> Either Error [With Loc Token]
tokenize = fmap (map locConvert) . mapLeft errorConvert . Lex.streamToEitherList . Lex.runLexer tokens "TODO filename" . textToString where
    errorConvert (Lex.LexicalError pos) = InvalidTokenAt pos
    locConvert (Loc.L loc token) = With loc token
