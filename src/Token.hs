module Token (Token (..), Keyword (..), Bracket (..), BracketKind (..), BracketDirection (..), Error (..), tokenize,
              With (..)) where

import MyPrelude

import Data.Char            (isAlpha, isAlphaNum, isDigit)
import Data.Functor.Compose (Compose (Compose, getCompose))

import qualified Data.Text   as Text
import qualified Text.Earley as E

import qualified Data.Loc.Span as Span
import Data.Loc (Span, spanFromTo, Loc, loc, locLine, locColumn)

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

data SpanMonoid
    = Span !Span
    | EmptySpan
    deriving (Generic, Show)

instance Semigroup SpanMonoid where
    Span s1   <> Span s2   = Span (Span.join s1 s2)
    Span s1   <> EmptySpan = Span s1
    EmptySpan <> Span s2   = Span s2
    EmptySpan <> EmptySpan = EmptySpan

instance Monoid SpanMonoid where
    mempty = EmptySpan

newtype With info a = With {
    unWith :: (info, a)
} deriving (Functor, Applicative, Show)

mapInfo :: (info1 -> info2) -> With info1 a -> With info2 a
mapInfo f (With (info, a)) = With (f info, a)

instance Eq a => Eq (With info a) where
    (==) = (==) `on` (snd . unWith)

type Expected = Text

type Prod r = Compose (E.Prod r Expected (With SpanMonoid Char)) (With SpanMonoid)

token :: Char -> Prod r Char
token = Compose . E.token . pure

satisfy :: (Char -> Bool) -> Prod r Char
satisfy = Compose . E.satisfy . (\f -> f . snd . unWith)

matchRepresentable :: TextRepresentation a => a -> Prod r a
matchRepresentable a = do
    _ <- (Compose . fmap pure . unused . E.list . map pure . textToString . toText) a
    return a

matchEnumerable :: (Enumerable a, TextRepresentation a) => (a -> Token) -> Prod r Token
matchEnumerable toToken = liftA1 toToken (oneOf (map matchRepresentable enumerate))

whitespace :: Prod r Char
whitespace = oneOf (map token [' ', '\n'])

whitespaced :: Prod r output -> Prod r output
whitespaced inner = do
    _      <- whitespace
    output <- inner
    _      <- whitespace
    return output

literal :: Token -> Prod r Token
literal = matchRepresentable

orUnderscore :: (Char -> Bool) -> (Char -> Bool)
orUnderscore f = \c -> f c || (c == '_')

name :: Prod r Text
name = do
    first <- satisfy (orUnderscore isAlpha)
    rest  <- zeroOrMore (satisfy (orUnderscore isAlphaNum))
    return (stringToText (first : rest))

nameOrKeyword :: Text -> Token
nameOrKeyword text' = case lookup text' (map (\keyword -> (toText keyword, keyword)) (enumerate @Keyword)) of
    Just keyword -> Keyword keyword
    Nothing      -> Name text'

number :: Prod r Integer
number = do
    minusSign <- zeroOrOne (token '-')
    digits    <- oneOrMore (satisfy isDigit)
    return (read (minusSign ++ digits))

text :: Prod r Text
text = do
    _       <- token '"'
    content <- zeroOrMore (satisfy (\c -> notElem c ['"', '\n']))
    _       <- token '"'
    return (stringToText content)

-- FIXME BUGS:
-- unary negation is ambiguous with both negative literals and binary negation :(
tokens :: E.Grammar r (E.Prod r Expected (With SpanMonoid Char) [With SpanMonoid Token])
tokens = mdo
    spaces     <- E.rule $ liftA1 (const []) (getCompose (oneOrMore whitespace))
    stringlike <- E.rule $ liftA1 single     (getCompose (oneOf [liftA1 nameOrKeyword name, liftA1 Number number]))
    fixed      <- E.rule $ liftA1 single     (getCompose (oneOf
        [whitespaced (matchEnumerable BinaryOperator),
        matchEnumerable UnaryOperator,
        matchEnumerable Bracket',
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

tokenize :: Text -> Either Error [With SpanMonoid Token]
tokenize = checkResult . tryParse . addSpans where
    addSpans    = map (mapInfo locToSpan) . zipWithLoc . textToString
    locToSpan l = Span (spanFromTo l (loc (locLine l) (locColumn l + 1)))
    tryParse    = fst . E.fullParses (E.parser tokens)
    checkResult = \case
        []    -> Left  Invalid
        [one] -> Right one
        more  -> Left  (Ambiguous (map (map (snd . unWith)) more))

zipWithLoc :: [Char] -> [With Loc Char]
zipWithLoc chars = map With (zip (scanl' stepLoc (loc 1 1) chars) chars) where
    stepLoc oldLoc = \case
        '\n' -> loc (locLine oldLoc + 1) 1
        _    -> loc (locLine oldLoc)     (locColumn oldLoc + 1)
