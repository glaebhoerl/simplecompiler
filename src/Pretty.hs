module Pretty (Info (..), IdentInfo (..), Style (..), Color(..), DefaultStyle(..), Render(..), Output(..), outputShow,
               note, keyword, colon, defineEquals, assignEquals, string, number, boolean, braces, parens, unaryOperator, binaryOperator,
               P.dquotes, P.hardline, P.hsep, P.nest, P.pretty, P.punctuate) where

import MyPrelude

import qualified Data.Text.Prettyprint.Doc                 as P
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PT

import qualified Data.ByteString

note :: a -> Doc a -> Doc a
note = P.annotate

keyword :: Text -> Doc (Info name)
keyword = note Keyword . P.pretty

colon :: Doc (Info name)
colon = note Colon ":"

defineEquals :: Doc (Info name)
defineEquals = note DefineEquals "="

assignEquals :: Doc (Info name)
assignEquals = note AssignEquals "="

string :: Text -> Doc (Info name)
string = note (Literal' TextLiteral) . P.dquotes . P.pretty

number :: (P.Pretty a, Integral a) => a -> Doc (Info name)
number = note (Literal' IntLiteral) . P.pretty

boolean :: Bool -> Doc (Info name)
boolean = note (Literal' BoolLiteral) . (\case True -> "true"; False -> "false")

braces :: Doc (Info name) -> Doc (Info name)
braces doc = note Brace "{" ++ doc ++ note Brace "}"

parens :: Doc (Info name) -> Doc (Info name)
parens doc = note Paren "(" ++ doc ++ note Paren ")"

-- FIXME: Deduplicate these with `module Token` maybe??
unaryOperator :: UnaryOperator -> Doc (Info name)
unaryOperator  = note UserOperator . \case
    Not                   -> "!"
    Negate                -> "-"

binaryOperator :: BinaryOperator -> Doc (Info name)
binaryOperator = note UserOperator . \case
    ArithmeticOperator op -> case op of
        Add               -> "+"
        Sub               -> "-"
        Mul               -> "*"
        Div               -> "/"
        Mod               -> "%"
    ComparisonOperator op -> case op of
        Equal             -> "=="
        NotEqual          -> "!="
        Less              -> "<"
        LessEqual         -> "<="
        Greater           -> ">"
        GreaterEqual      -> ">="
    LogicalOperator    op -> case op of
        And               -> "&&"
        Or                -> "||"

-- TODO bikeshed the names of all these things

data Info name
    = Keyword
    | Brace
    | Paren
    | DefineEquals
    | AssignEquals
    | Colon
    | UserOperator
    | Literal'   !LiteralType
    | Sigil      !(IdentInfo name)
    | Identifier !(IdentInfo name)
    deriving (Generic, Eq, Show)

data LiteralType
    = IntLiteral
    | BoolLiteral
    | TextLiteral
    deriving (Generic, Eq, Show)

data IdentInfo name = IdentInfo {
    isDefinition :: !Bool,
    identName    :: !name
} deriving (Generic, Eq, Show)

data Style = Style {
    color        :: !(Maybe Color),
    isDull       :: !Bool,
    isBold       :: !Bool,
    isItalic     :: !Bool,
    isUnderlined :: !Bool
} deriving (Generic, Eq, Show)

data Color
    = Black
    | White
    | Red
    | Green
    | Blue
    | Cyan
    | Magenta
    | Yellow
    deriving (Generic, Eq, Show)

class DefaultStyle a where
    applyStyle :: Style -> a -> Style

instance DefaultStyle name => DefaultStyle (Info name) where
    applyStyle base = \case
        Keyword          -> base { isBold = True }
        Brace            -> base { isBold = True }
        Paren            -> base
        DefineEquals     -> base { isBold = True }
        AssignEquals     -> base { color  = Just Yellow }
        Colon            -> base { isBold = True }
        UserOperator     -> base { color  = Just Yellow }
        Literal'   _     -> base { color  = Just Red } -- TODO ability to customize per-type?
        Sigil      info  -> base { isUnderlined = isDefinition info } -- also do applyStyle (identName info) if we want colored sigils
        Identifier info  -> applyStyle (base { isUnderlined = isDefinition info }) (identName info)

class Render a where
    type Name a
    render :: a -> Doc (Info (Name a))

class Output a where
    output :: Handle -> a -> IO ()
    default output :: (Render a, DefaultStyle (Name a)) => Handle -> a -> IO ()
    output handle = PT.hPutDoc handle . fmap (ansiStyle . applyStyle plain) . render

plain :: Style
plain = Style Nothing False False False False

outputShow :: Show a => Handle -> a -> IO ()
outputShow handle = hPutStr handle . prettyShow

instance Output Text where
    output = hPutStr

instance Output ByteString where
    output = Data.ByteString.hPutStr

instance Show a => Output [a] where -- shouldn't we have Output as the superclass constraint somehow...?
    output = outputShow

ansiStyle :: Style -> PT.AnsiStyle
ansiStyle Style { color, isDull, isBold, isItalic, isUnderlined } = style where
    style     = maybe mempty (fromColor . mapColor) color ++ fontStyle
    fontStyle = mconcat (catMaybes [justIf isBold PT.bold, justIf isItalic PT.italicized, justIf isUnderlined PT.underlined])
    fromColor = if isDull then PT.colorDull else PT.color
    mapColor  = \case
        Black   -> PT.Black
        White   -> PT.White
        Red     -> PT.Red
        Green   -> PT.Green
        Blue    -> PT.Blue
        Cyan    -> PT.Cyan
        Magenta -> PT.Magenta
        Yellow  -> PT.Yellow
