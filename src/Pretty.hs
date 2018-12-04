module Pretty (Document, Info (..), IdentInfo (..), DefinitionOrUse (..), Type (..), Style (..), Color (..), Render (..), output,
               note, keyword, dot, colon, semicolon, defineEquals, assignEquals, string, number, boolean, braces, parens, unaryOperator, binaryOperator,
               P.dquotes, P.hardline, P.hsep, P.nest, P.pretty, P.punctuate) where

import MyPrelude

import qualified Data.Text.Prettyprint.Doc                 as P
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PT

import qualified Data.ByteString


type Document = P.Doc Info

note :: Info -> Document -> Document
note = P.annotate

keyword :: Text -> Document
keyword = note Keyword . P.pretty

dot :: Document
dot = note Dot "."

colon :: Document
colon = note Colon ":"

semicolon :: Document
semicolon = note Semicolon ";"

defineEquals :: Document
defineEquals = note DefineEquals "="

assignEquals :: Document
assignEquals = note AssignEquals "="

string :: Text -> Document
string = note (Literal Text) . P.dquotes . P.pretty

number :: (P.Pretty a, Integral a) => a -> Document
number = note (Literal Int) . P.pretty . (+ 0) -- just to silence the "unused constraint" warning

boolean :: Bool -> Document
boolean = note (Literal Bool) . bool "false" "true"

braces :: Document -> Document
braces doc = note Brace "{" ++ doc ++ note Brace "}"

parens :: Document -> Document
parens doc = note Paren "(" ++ doc ++ note Paren ")"

-- FIXME: Deduplicate these with `module Token` maybe??
unaryOperator :: UnaryOperator -> Document
unaryOperator  = note UserOperator . \case
    Not                   -> "!"
    Negate                -> "-"

binaryOperator :: BinaryOperator -> Document
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

data Info
    = Keyword
    | Brace
    | Paren
    | Bracket
    | DefineEquals
    | AssignEquals
    | Dot
    | Colon
    | Semicolon
    | UserOperator
    | Literal    Type
    | Sigil      IdentInfo
    | Identifier IdentInfo
    deriving (Generic, Eq, Show)

data Type
    = Int
    | Bool
    | Text
    | Unit
    | Function
    | Block
    | Type
    | Unknown
    deriving (Generic, Eq, Show)

data DefinitionOrUse
    = Definition
    | Use
    deriving (Generic, Eq, Show)

data IdentInfo = IdentInfo {
    identName :: Text,
    defOrUse  :: DefinitionOrUse,
    identType :: Type,
    isBuiltin :: Bool
} deriving (Generic, Eq, Show)

data Style = Style {
    color        :: Maybe Color,
    isDull       :: Bool,
    isBold       :: Bool,
    isItalic     :: Bool,
    isUnderlined :: Bool
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

defaultStyle :: Info -> Style
defaultStyle = \case
    Keyword          -> plain { isBold = True }
    Brace            -> plain { isBold = True }
    Paren            -> plain
    Bracket          -> plain
    DefineEquals     -> plain { isBold = True }
    AssignEquals     -> plain { color  = Just Yellow }
    Dot              -> plain { isBold = True }
    Colon            -> plain { isBold = True }
    Semicolon        -> plain { isBold = True }
    UserOperator     -> plain { color  = Just Yellow }
    Literal    _     -> plain { color  = Just Red }
    Sigil      info  -> plain { isUnderlined = defOrUse info == Definition }
    Identifier info  -> plain { isUnderlined = defOrUse info == Definition, color = Just (identColorForType (identType info)) }
        where identColorForType = \case
                  Unknown  -> Cyan
                  Function -> if isBuiltin info then Yellow else Green
                  Block    -> Green
                  Type     -> Cyan
                  _        -> if isBuiltin info then Red else Magenta

plain :: Style
plain = Style Nothing False False False False

class Render a where
    render :: a -> Document
    outputWithStyle :: (Info -> Style) -> Handle -> a -> IO ()
    outputWithStyle style handle = PT.hPutDoc handle . fmap (ansiStyle . style) . render

output :: Render a => Handle -> a -> IO ()
output = outputWithStyle defaultStyle

instance Render Text where
    render = P.pretty
    outputWithStyle _ = hPutStr

instance Render ByteString where
    render = P.pretty . byteStringToText
    outputWithStyle _ = Data.ByteString.hPutStr

instance Render b => Render (With a b) where
    render = render . unWith

instance Render (node a b) => Render (NodeWith node a b) where
    render = render . nodeWithout

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
