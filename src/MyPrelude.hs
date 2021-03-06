module MyPrelude (module MyPrelude, module Reexports) where



-------------------------------------------------------------------------- reexports

import Prelude                          as Reexports hiding (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile, head, tail, (++), foldl, scanl, (/=), ($), ($!))
import Data.Text.IO                     as Reexports        (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile, hGetContents, hPutStr, hPutStrLn)
import System.IO                        as Reexports        (Handle, FilePath, IOMode (ReadMode, WriteMode, AppendMode, ReadWriteMode), stdin, stdout, stderr, withFile)
import Data.Foldable                    as Reexports        (foldl')
import Data.Int                         as Reexports        ( Int,  Int8,  Int16,  Int32,  Int64)
import Data.Word                        as Reexports        (Word, Word8, Word16, Word32, Word64)
import Data.Either                      as Reexports        (isLeft, isRight, fromLeft, fromRight)
import Data.Maybe                       as Reexports        (isJust, isNothing, fromMaybe, maybeToList, catMaybes, mapMaybe)
import Data.List                        as Reexports        (scanl', uncons, intercalate)
import Data.Function                    as Reexports        (on)
import Control.Applicative              as Reexports        (Alternative (empty), liftA2, liftA3)
import Control.Monad                    as Reexports        (liftM, forM, forM_, zipWithM, zipWithM_, foldM, foldM_, filterM, replicateM, forever, join, guard, when, unless)
import Control.Monad.Trans              as Reexports        (MonadTrans (lift))
import Control.Monad.IO.Class           as Reexports        (MonadIO    (liftIO))
import Control.Monad.Managed.Safe       as Reexports        (Managed, MonadManaged (using), managed, managed_, runManaged)
import Control.Monad.Except             as Reexports        (ExceptT, Except, MonadError,  throwError, catchError, runExceptT, runExcept)
import Control.Monad.Reader             as Reexports        (ReaderT, Reader, MonadReader, ask, local)
import Control.Monad.Writer.Strict      as Reexports        (WriterT, Writer, MonadWriter, tell, runWriterT, runWriter, execWriterT, execWriter)
import Control.Monad.State.Strict       as Reexports        (StateT,  State,  MonadState)
import Data.ByteString                  as Reexports        (ByteString)
import Data.Text                        as Reexports        (Text, toLower, toUpper)
import Data.Text.Prettyprint.Doc        as Reexports        (Doc)
import Data.Set                         as Reexports        (Set)
import Data.Map.Strict                  as Reexports        (Map)
import GHC.Generics                     as Reexports        (Generic)
import Data.Generics.Product.Fields     as Reexports        (HasField')
import Data.Generics.Sum.Constructors   as Reexports        (AsConstructor')
import Data.Loc                         as Reexports        (Loc)



-------------------------------------------------------------------------- local imports

import qualified Prelude
import qualified Text.Pretty.Simple
import qualified Data.Text                      as Text
import qualified Data.Text.Encoding             as Text
import qualified Data.Text.Lazy                 as LazyText
import qualified Control.Monad.Reader           as Reader      (runReaderT, runReader)
import qualified Control.Monad.State.Strict     as State       (runStateT,  runState,  evalStateT,  evalState,  execStateT,  execState, get, put)
import qualified Data.Generics.Product.Fields   as GenericLens (field')
import qualified Data.Generics.Sum.Constructors as GenericLens (_Ctor')
import Control.Applicative   (some, many, Const (Const, getConst), (<|>))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Profunctor (Profunctor (lmap, rmap), Choice (right'))

import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified GHC.Stack as Stack
import qualified Debug.Trace


-------------------------------------------------------------------------- prelude replacements

head :: [a] -> Maybe a
head = \case
    []  -> Nothing
    a:_ -> Just a

tail :: [a] -> Maybe [a]
tail = \case
    []   -> Nothing
    _:as -> Just as

(++) :: Semigroup a => a -> a -> a
(++) = (Prelude.<>)

(!=) :: Eq a => a -> a -> Bool
(!=) = (Prelude./=)

strictly :: (a -> b) -> (a -> b)
strictly = (Prelude.$!)


-------------------------------------------------------------------------- other utility functions

(%) :: Integral num => num -> num -> num
(%) = mod

bool :: a -> a -> Bool -> a
bool false true b = if b then true else false

at :: Int -> [a] -> Maybe a
at pos = head . drop pos

prepend :: a -> [a] -> [a]
prepend = (:)

single :: a -> [a]
single = \a -> [a]

singleIf ::  Bool -> a -> [a]
singleIf = pureIf -- see below

justIf :: Bool -> a -> Maybe a
justIf = pureIf

asLeft :: Maybe a -> Either a ()
asLeft = maybe (Right ()) Left

asRight :: Maybe a -> Either () a
asRight = reflection . asLeft

left :: Either a b -> Maybe a
left = either Just (const Nothing)

right :: Either a b -> Maybe b
right = left . reflection

fromLeftOr :: (b -> a) -> Either a b -> a
fromLeftOr f = whichever . fmap f

fromRightOr :: (a -> b) -> Either a b -> b
fromRightOr f = fromLeftOr f . reflection

reflection :: Either a b -> Either b a
reflection = either Right Left

whichever :: Either a a -> a
whichever = either id id

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f = reflection . fmap f . reflection



-------------------------------------------------------------------------- Applicative and Alternative

liftA0 :: Applicative f => a -> f a
liftA0 = pure

liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 = fmap

pureIf :: Alternative f => Bool -> a -> f a
pureIf b a = if b then pure a else empty

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl' (<|>) empty

-- sometimes we want the Maybe version, sometimes we want the list version...
-- (`liftA1 head (zeroOrOne x)` recovers the Maybe version)
zeroOrOne :: Alternative f => f a -> f [a]
zeroOrOne a = oneOf [liftA0 [], liftA1 single a]

oneOrMore :: Alternative f => f a -> f [a]
oneOrMore = some

zeroOrMore :: Alternative f => f a -> f [a]
zeroOrMore = many



-------------------------------------------------------------------------- Monad

ifM :: Monad m => m Bool -> m a -> m (Maybe a)
ifM condition action = do
    result <- condition
    if result
        then liftM Just action
        else return Nothing

elseM :: Monad m => m (Maybe a) -> m a -> m a
elseM thenAction elseAction = do
    result <- thenAction
    case result of
        Just a  -> return a
        Nothing -> elseAction

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition = unused . ifM condition

whileM :: Monad m => m Bool -> m ()
whileM condition = whenM condition (whileM condition)

whileJustM :: Monad m => a -> (a -> m (Maybe a)) -> m ()
whileJustM a action = whileRightM a (liftM asRight . action . whichever)

whileRightM :: Monad m => a -> (Either a c -> m (Either b c)) -> m b
whileRightM = impl . Left where
    impl input action = do
        result <- action input
        case result of
            Right c -> impl (Right c) action
            Left  b -> return b

unfoldM :: Monad m => m (Maybe a) -> m [a]
unfoldM action = do
    result <- action
    case result of
        Just a  -> liftM (prepend a) (unfoldM action)
        Nothing -> return []

try :: MonadError e m => Either e a -> m a
try = either throwError return

tryM :: MonadError e m => m (Either e a) -> m a
tryM action = do
    result <- action
    try result

usingManaged :: MonadManaged m => (forall r. (a -> IO r) -> IO r) -> m a
usingManaged with = using (managed with)

unused :: Functor m => m a -> m ()
unused = fmap (const ())



-------------------------------------------------------------------------- lenses and prisms

-- TODO
-- composing a lens with a prism gives a traversal, we should probably have that too
-- which functions can/should be generalized?

type Lens  outer inner = forall    f.             Functor     f  => (inner  ->  f inner) -> (outer  ->  f outer)

type Prism outer inner = forall to f. (Choice to, Applicative f) => (inner `to` f inner) -> (outer `to` f outer)

get :: Lens outer inner -> outer -> inner
get lens outer = getConst (lens Const outer)

set :: Lens outer inner -> inner -> outer -> outer
set lens inner outer = runIdentity (lens (const (Identity inner)) outer)

modify :: Lens outer inner -> (inner -> inner) -> (outer -> outer)
modify lens f outer = set lens (f (get lens outer)) outer

getWhen :: Prism outer inner -> outer -> Maybe inner
getWhen prism outer = right (snd (unPrism prism) outer)

is :: outer -> Prism outer inner -> Bool
is outer prism = isJust (getWhen prism outer)

isn't :: outer -> Prism outer inner -> Bool
isn't outer prism = not (is outer prism)

constructFrom :: Prism outer inner -> inner -> outer
constructFrom prism inner = fst (unPrism prism) inner

modifyWhen :: Prism outer inner -> (inner -> inner) -> (outer -> outer)
modifyWhen prism f outer = maybe outer (constructFrom prism . f) (getWhen prism outer)

field :: forall name inner outer. HasField' name outer inner => Lens outer inner
field = GenericLens.field' @name

constructor :: forall name inner outer. AsConstructor' name outer inner => Prism outer inner
constructor = GenericLens._Ctor' @name

-- for convenience
match :: forall name inner outer. AsConstructor' name outer inner => outer -> Maybe inner
match = getWhen (constructor @name)

{- I wanted to use `#foo` instead of `@"foo"` syntax, using OverloadedLabels, but turns out it doesn't allow uppercase labels (for constructors) :(

data Field (name :: Symbol) = Field
data Case  (name :: Symbol) = Case

instance name1 ~ name2 => GHC.IsLabel name1 (Field name2) where
    fromLabel _ = Field

instance name1 ~ name2 => GHC.IsLabel name1 (Case name2) where
    fromLabel _ = Case

field :: forall name inner outer. GenericLens.HasField      name inner outer => Field name -> Lens  outer inner
field Field = GenericLens.field @name

_Case :: forall name inner outer. GenericLens.AsConstructor name inner outer => Case  name -> Prism outer inner
_Case Case  = GenericLens._Ctor @name
-}


-- copied from https://artyom.me/lens-over-tea-5
-- TODO maybe rewrite these
-- TODO don't export them
data Market a b s t = Market (b -> t) (s -> Either t a)

instance Functor (Market a b s) where
  fmap f (Market bt seta) = Market (f . bt) (either (Left . f) Right . seta)

instance Profunctor (Market a b) where
  lmap f (Market bt seta) = Market bt (seta . f)
  rmap f (Market bt seta) = fmap f (Market bt seta)

instance Choice (Market a b) where
  right' (Market bt seta) = Market (Right . bt) \cs -> case cs of
    Left c -> Left (Left c)
    Right s -> case seta s of
      Left t -> Left (Right t)
      Right a -> Right a

unPrism :: Prism a b -> (b -> a, a -> Either a b)
unPrism p =
  let -- bft   :: b -> Identity t
      -- setfa :: s -> Either (Identity t) a
      Market bft setfa = p (Market Identity Right)
      -- bt    :: b -> t
      -- seta  :: s -> Either t a
      bt = runIdentity . bft
      seta = either (Left . runIdentity) Right . setfa
  in (bt, seta)



-------------------------------------------------------------------------- reader & state monad

runReaderT :: r -> ReaderT r m a -> m a
runReaderT = flip Reader.runReaderT

runReader :: r -> Reader r a -> a
runReader = flip Reader.runReader

runStateT :: s -> StateT s m a -> m (a, s)
runStateT = flip State.runStateT

runState :: s -> State s a -> (a, s)
runState = flip State.runState

evalStateT :: Monad m => s -> StateT s m a -> m a
evalStateT = flip State.evalStateT

evalState :: s -> State s a -> a
evalState = flip State.evalState

execStateT ::  Monad m => s -> StateT s m a -> m s
execStateT = flip State.execStateT

execState :: s -> State s a -> s
execState = flip State.execState

-- these are renamed to avoid conflicts with lens get/set, above
getState :: MonadState s m => m s
getState = State.get

-- these could be defined in terms of `modifyState`/`doModifyState`, except they are the actual primitives of `MonadState`!
setState :: MonadState s m => s -> m ()
setState = State.put

doSetState :: MonadState s m => m s -> m ()
doSetState = unused . doModifyState . const

modifyState :: MonadState s m => (s -> s) -> m s
modifyState f = doModifyState (return . f)

doModifyState :: MonadState s m => (s -> m s) -> m s
doModifyState modifyAction = do
    oldState <- getState
    newState <- modifyAction oldState
    strictly setState newState
    return newState

setM :: MonadState outer m => Lens outer inner -> inner -> m ()
setM lens = doSetM lens . return

doSetM :: MonadState outer m => Lens outer inner -> m inner -> m ()
doSetM lens = unused . doModifyM lens . const

getM :: MonadState outer m => Lens outer inner -> m inner
getM lens = modifyM lens id

modifyM :: MonadState outer m => Lens outer inner -> (inner -> inner) -> m inner
modifyM lens f = doModifyM lens (return . f)

doModifyM :: MonadState outer m => Lens outer inner -> (inner -> m inner) -> m inner
doModifyM lens modifyAction = liftM (get lens) (doModifyState (lens modifyAction))

getWhenM :: MonadState outer m => Prism outer inner -> m (Maybe inner)
getWhenM prism = modifyWhenM prism id

constructFromM :: MonadState outer m => Prism outer inner -> inner -> m ()
constructFromM prism = doConstructFromM prism . return

doConstructFromM :: MonadState outer m => Prism outer inner -> m inner -> m ()
doConstructFromM prism = unused . doModifyWhenM prism . const

modifyWhenM :: MonadState outer m => Prism outer inner -> (inner -> inner) -> m (Maybe inner)
modifyWhenM prism f = doModifyWhenM prism (return . f)

doModifyWhenM :: MonadState outer m => Prism outer inner -> (inner -> m inner) -> m (Maybe inner)
doModifyWhenM prism modifyAction = liftM (getWhen prism) (doModifyState (prism modifyAction))

-- TODO `zoom` maybe?

(+=) :: (MonadState outer m, Num inner) => Lens outer inner -> inner -> m inner
lens += n = modifyM lens (+ n)

(-=) :: (MonadState outer m, Num inner) => Lens outer inner -> inner -> m inner
lens -= n = modifyM lens (subtract n)

(*=) :: (MonadState outer m, Num inner) => Lens outer inner -> inner -> m inner
lens *= n = modifyM lens (* n)

(/=) :: (MonadState outer m, Fractional inner) => Lens outer inner -> inner -> m inner
lens /= n = modifyM lens (/ n)

infixr 4 +=, -=, *=, /=

-- we could have `%=`, but with *both* `%` and `%=` having different meanings relative to Haskell convention, it's probably too surprising



-------------------------------------------------------------------------- Text stuff

type LazyText = LazyText.Text

showText :: Show a => a -> Text
showText = stringToText . show

prettyShow :: Show a => a -> Text
prettyShow = LazyText.toStrict . Text.Pretty.Simple.pShowLightBg

prettyPrint :: Show a => a -> IO ()
prettyPrint = Text.Pretty.Simple.pPrintLightBg

stringToText :: String -> Text
stringToText = Text.pack

textToString :: Text -> String
textToString = Text.unpack

byteStringToText :: ByteString -> Text
byteStringToText = Text.decodeUtf8

textToByteString :: Text -> ByteString
textToByteString = Text.encodeUtf8




-------------------------------------------------------------------------- asserts and debugging

{-# WARNING todo "TODO" #-}
todo :: HasCallStack => a
todo = error "TODO"

bug :: HasCallStack => Text -> a
bug x = error (textToString ("BUG: " ++ x))

class Assert x where
    type AssertResult x
    msgAssert :: HasCallStack => Text -> x -> AssertResult x

assert :: (HasCallStack, Assert x) => x -> AssertResult x
assert = withFrozenCallStack
    (msgAssert "")

assertM :: (HasCallStack, Assert x, Monad m) => x -> m (AssertResult x)
assertM x = withFrozenCallStack
    (strictly return (assert x))

msgAssertM :: (HasCallStack, Assert x, Monad m) => Text -> x -> m (AssertResult x)
msgAssertM msg x = withFrozenCallStack
    (strictly return (msgAssert msg x))

assertEqM :: (HasCallStack, Eq a, Show a, Monad m) => a -> a -> m ()
assertEqM a b = withFrozenCallStack
    (msgAssertM (showText a ++ " == " ++ showText b) (a == b))

instance Assert Bool where
    type AssertResult Bool = ()
    msgAssert msg = withFrozenCallStack
        (bool (bug ("Failed assertion! " ++ msg)) ())

instance Assert (Maybe a) where
    type AssertResult (Maybe a) = a
    msgAssert msg = withFrozenCallStack
        (fromMaybe (bug ("Failed assertion! " ++ msg)))

{- remove the Show constraint if it turns out to be problematic! -}
instance Show e => Assert (Either e a) where
    type AssertResult (Either e a) = a
    msgAssert msg = withFrozenCallStack
        (fromRightOr (\e -> bug ("Failed assertion! " ++ msg ++ " " ++ showText e)))

debug :: (HasCallStack, Show a) => a -> a
debug a = withFrozenCallStack
    (trace (showText a) a)

debugM :: (HasCallStack, Monad m, Show a) => a -> m ()
debugM a = withFrozenCallStack
    (traceM (showText a))

prettyDebug :: (HasCallStack, Show a) => a -> a
prettyDebug a = withFrozenCallStack
    (trace (prettyShow a) a)

prettyDebugM :: (HasCallStack, Monad m, Show a) => a -> m ()
prettyDebugM a = withFrozenCallStack
    (traceM (prettyShow a))

trace :: HasCallStack => Text -> a -> a
trace text a = Debug.Trace.trace message a where
    message = "DEBUG: " ++ textToString text ++ " [" ++ srcLoc ++ "]"
    Stack.SrcLoc { Stack.srcLocFile, Stack.srcLocStartLine, Stack.srcLocStartCol } = snd (assert (head (Stack.getCallStack Stack.callStack)))
    srcLoc = srcLocFile ++ ":" ++ show srcLocStartLine ++ ":" ++ show srcLocStartCol

traceM :: (HasCallStack, Monad m) => Text -> m ()
traceM text = withFrozenCallStack
    (trace text (return ()))




-------------------------------------------------------------------------- widely-used project-specific definitions

class Enumerable a where
    enumerate :: [a]
    default enumerate :: (Enum a, Bounded a) => [a]
    enumerate = [minBound..maxBound]

data ArithmeticOperator
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable ArithmeticOperator

data ComparisonOperator
    = Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Equal
    | NotEqual
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable ComparisonOperator

data LogicalOperator
    = And
    | Or
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable LogicalOperator

data BinaryOperator
    = ArithmeticOperator ArithmeticOperator
    | ComparisonOperator ComparisonOperator
    | LogicalOperator    LogicalOperator
    deriving (Generic, Eq, Show)

instance Enumerable BinaryOperator where
    enumerate = map ArithmeticOperator enumerate ++ map ComparisonOperator enumerate ++ map LogicalOperator enumerate

data UnaryOperator
    = Not
    | Negate
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable UnaryOperator


data With metadata a = With {
    getMetadata :: metadata,
    unWith      :: a
} deriving (Generic, Show, Functor, Foldable, Traversable)

instance Eq a => Eq (With metadata a) where
    (==) = (==) `on` unWith

instance Ord a => Ord (With metadata a) where
    compare = compare `on` unWith

instance Monoid metadata => Applicative (With metadata) where
    pure = With mempty
    liftA2 f (With a1 b1) (With a2 b2) = With (a1 ++ a2) (f b1 b2)

newtype NodeWith node metadata a = NodeWith {
    getNodeWith :: With metadata (node metadata a)
} deriving (Generic, Eq, Ord, Show, Functor, Foldable, Traversable)

nodeWithout :: NodeWith node metadata a -> node metadata a
nodeWithout = unWith . getNodeWith

nodeMetadata :: NodeWith node metadata a -> metadata
nodeMetadata = getMetadata . getNodeWith

mapNode :: (node metadata a -> node metadata b) -> NodeWith node metadata a -> NodeWith node metadata b
mapNode f = NodeWith . fmap f . getNodeWith

mapNodeM :: Monad m => (node metadata a -> m (node metadata b)) -> NodeWith node metadata a -> m (NodeWith node metadata b)
mapNodeM f = liftM NodeWith . mapM f . getNodeWith

forNodeM :: Monad m => NodeWith node metadata a -> (node metadata a -> m (node metadata b)) -> m (NodeWith node metadata b)
forNodeM = flip mapNodeM

-- Workaround for bug with the `Alternative` instance in base: https://ghc.haskell.org/trac/ghc/ticket/15992
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Generic, Eq, Ord, Show, Functor, Foldable)

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose (liftA2 (<*>) f x)
    liftA2 f (Compose x) (Compose y) =
      Compose (liftA2 (liftA2 f) x y)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose empty
    Compose a <|> Compose b = Compose (a <|> b)
    many = Compose . liftA1 sequenceA . many . getCompose
    some = Compose . liftA1 sequenceA . some . getCompose
