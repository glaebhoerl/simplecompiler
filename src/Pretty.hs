module Pretty (Info (..), IdentInfo (..), Style (..), Color(..), DefaultStyle(..), Render(..), Output(..), outputShow,
               P.annotate, P.dquotes, P.hardline, P.hsep, P.nest, P.pretty, P.punctuate) where
-- TODO don't mass-reexport?

import MyPrelude

import qualified Data.Text.Prettyprint.Doc                 as P
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PT

import qualified Data.ByteString

-- TODO bikeshed the names of all these things

data Info literalType name
    = Keyword
    | Brace
    | Paren
    | DefineEquals
    | AssignEquals
    | Colon
    | UserOperator
    | Literal'   !literalType
    | Sigil      !(IdentInfo name)
    | Identifier !(IdentInfo name)
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

instance (DefaultStyle literalType, DefaultStyle name) => DefaultStyle (Info literalType name) where
    applyStyle base = \case
        Keyword          -> base { isBold = True }
        Brace            -> base { isBold = True }
        Paren            -> base
        DefineEquals     -> base { isBold = True }
        AssignEquals     -> base { color  = Just Yellow }
        Colon            -> base { isBold = True }
        UserOperator     -> base { color  = Just Yellow }
        Literal'   lit   -> applyStyle (base { color  = Just Red }) lit
        Sigil      info  -> base { isUnderlined = isDefinition info } -- also do applyStyle (identName info) if we want colored sigils
        Identifier info  -> applyStyle (base { isUnderlined = isDefinition info }) (identName info)

class Render a where
    type InfoFor a
    render :: a -> Doc (InfoFor a)

class Output a where
    output :: Handle -> a -> IO ()
    default output :: (Render a, DefaultStyle (InfoFor a)) => Handle -> a -> IO ()
    output handle = PT.hPutDoc handle . fmap (ansiStyle . (applyStyle plain)) . render

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
