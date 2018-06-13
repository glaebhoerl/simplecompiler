module Token (Token (..), Keyword (..), Bracket (..), BracketKind (..), BracketDirection (..), Error (..), tokenize) where

import MyPrelude

import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Text   as Text
import qualified Text.Earley as E

import qualified Pretty as P

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
    | K_else
    | K_forever
    | K_if
    | K_let
    | K_return
    | K_var
    | K_while
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable Keyword

instance TextRepresentation Keyword where
    toText = Text.pack . drop 2 . show

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
    bracketKind      :: !BracketKind,
    bracketDirection :: !BracketDirection
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
    = Keyword        !Keyword
    | Name           !Text
    | BinaryOperator !BinaryOperator
    | UnaryOperator  !UnaryOperator
    | Bracket'       !Bracket
    | Number         !Integer
    | Text           !Text
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
        Name           name'   -> P.note (P.Identifier (P.IdentInfo name' False P.UnresolvedName Nothing)) (P.pretty name')
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

literal :: Token -> Prod r Token
literal tok = do
    _ <- E.listLike (toText tok)
    return tok

orUnderscore :: (Char -> Bool) -> (Char -> Bool)
orUnderscore f = \c -> f c || (c == '_')

name :: Prod r Text
name = do
    first <- (E.satisfy (orUnderscore isAlpha))
    rest  <- (zeroOrMore (E.satisfy (orUnderscore isAlphaNum)))
    return (Text.pack (first : rest))

nameOrKeyword :: Text -> Token
nameOrKeyword text' = case lookup text' (map (\keyword -> (toText keyword, keyword)) (enumerate @Keyword)) of
    Just keyword -> Keyword keyword
    Nothing      -> Name text'

number :: Prod r Integer
number = do
    minusSign <- zeroOrOne (E.token '-')
    digits    <- oneOrMore (E.satisfy isDigit)
    return (read (minusSign ++ digits))

text :: Prod r Text
text = do
    _       <- E.token '"'
    content <- zeroOrMore (E.satisfy (\c -> notElem c ['"', '\n']))
    _       <- E.token '"'
    return (Text.pack content)

-- FIXME BUGS:
-- unary negation is ambiguous with both negative literals and binary negation :(
tokens :: Grammar r [Token]
tokens = mdo
    spaces     <- E.rule (liftA1 (const []) (oneOrMore (oneOf [E.token ' ', E.token '\n'])))
    stringlike <- E.rule (liftA1 single (oneOf [liftA1 nameOrKeyword name, liftA1 Number number]))
    fixed      <- E.rule (liftA1 single (oneOf
        [whitespaced (match BinaryOperator),
        match UnaryOperator,
        match Bracket',
        whitespaced (literal EqualsSign),
        literal Comma,
        literal Colon,
        literal Semicolon,
        liftA1 Text text]))
    spacesRec     <- E.rule (oneOf [spaces,     liftA2 (++) spaces     stringlikeRec, liftA2 (++) spaces     fixedRec])
    stringlikeRec <- E.rule (oneOf [stringlike, liftA2 (++) stringlike fixedRec,      liftA2 (++) stringlike spacesRec])
    fixedRec      <- E.rule (oneOf [fixed,      liftA2 (++) fixed      spacesRec,     liftA2 (++) fixed      stringlikeRec, liftA2 (++) fixed fixedRec])
    toks          <- E.rule (oneOf [spacesRec, stringlikeRec, fixedRec])
    return toks

data Error
    = Invalid
    | Ambiguous ![[Token]]
    deriving (Generic, Show)

tokenize :: Text -> Either Error [Token]
tokenize text' = case fst (E.fullParses (E.parser tokens) text') of
    []    -> Left  Invalid
    [one] -> Right one
    more  -> Left  (Ambiguous more)
