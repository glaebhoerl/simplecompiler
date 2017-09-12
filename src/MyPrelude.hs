{-# LANGUAGE AllowAmbiguousTypes #-} -- necessary to be able to make an alias for `Data.Generics.Sum.Constructors._Ctor`, apparently? :/

module MyPrelude (module MyPrelude, module Reexports) where



-------------------------------------------------------------------------- reexports

import Prelude                          as Reexports hiding (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile, head, tail, (++), foldl, (/=))
import Data.Text.IO                     as Reexports        (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile)
import Data.Foldable                    as Reexports        (foldl')
import Data.Int                         as Reexports        ( Int,  Int8,  Int16,  Int32,  Int64)
import Data.Word                        as Reexports        (Word, Word8, Word16, Word32, Word64)
import Data.Either                      as Reexports        (isLeft, isRight{-, fromLeft, fromRight-})
import Data.Maybe                       as Reexports        (isJust, isNothing, fromMaybe, maybeToList, catMaybes, mapMaybe)
import Data.Function                    as Reexports        (fix, on)
import Control.Applicative              as Reexports        (Alternative (empty, (<|>)), liftA2, liftA3)
import Control.Monad                    as Reexports        (liftM, forM, forM_, (>=>), (<=<), forever, void, join, filterM, foldM, zipWithM, replicateM, guard, when, unless)
import Control.Monad.Trans              as Reexports        (MonadTrans (lift))
import Control.Monad.Except             as Reexports        (ExceptT, Except, MonadError, throwError, catchError, runExceptT, runExcept)
import Control.Monad.State.Strict       as Reexports        (StateT,  State,  MonadState)
import Data.Text                        as Reexports        (Text)
import Data.Set                         as Reexports        (Set)
import Data.Map.Strict                  as Reexports        (Map)
import GHC.Generics                     as Reexports        (Generic)
import Data.Generics.Product.Fields     as Reexports        (field)



-------------------------------------------------------------------------- local imports

import qualified Prelude ((/=))
import qualified Text.Pretty.Simple
import qualified Data.Text                      as Text
import qualified Data.Text.Lazy                 as LazyText
import qualified Control.Monad.State.Strict     as State       (runStateT, runState, evalStateT, evalState, execStateT, execState, get, put, modify')
import qualified Data.Generics.Sum.Constructors as GenericLens (AsConstructor, _Ctor)
import Control.Applicative   (some, many, Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Profunctor (Profunctor (lmap, rmap), Choice (right'))

import GHC.Stack (HasCallStack)



-------------------------------------------------------------------------- prelude replacements

head :: [a] -> Maybe a
head = \case
    []  -> Nothing
    a:_ -> Just a

tail :: [a] -> Maybe [a]
tail = \case
    []   -> Nothing
    _:as -> Just as

(++) :: Monoid a => a -> a -> a
(++) = mappend

(!=) :: Eq a => a -> a -> Bool
(!=) = (Prelude./=)



-------------------------------------------------------------------------- other utility functions

prepend :: a -> [a] -> [a]
prepend = (:)

single :: a -> [a]
single = \a -> [a]

left :: Either a b -> Maybe a
left = either Just (const Nothing)

right :: Either a b -> Maybe b
right = either (const Nothing) Just

fromLeft :: a -> Either a b -> a
fromLeft r = fromLeftOr (const r)

fromRight :: b -> Either a b -> b
fromRight l = fromRightOr (const l)

fromLeftOr :: (b -> a) -> Either a b -> a
fromLeftOr f = either id f

fromRightOr :: (a -> b) -> Either a b -> b
fromRightOr f = either f id

bool :: a -> a -> Bool -> a
bool false true b = if b then true else false



-------------------------------------------------------------------------- Applicative and Alternative

liftA0 :: Applicative f => a -> f a
liftA0 = pure

liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 = fmap

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl' (<|>) empty

-- sometimes we want the Maybe version, sometimes we want the list version...
zeroOrOne :: Alternative f => f a -> f [a]
zeroOrOne a = liftA0 [] <|> liftA1 single a

oneOrMore :: Alternative f => f a -> f [a]
oneOrMore = some

zeroOrMore :: Alternative f => f a -> f [a]
zeroOrMore = many



-------------------------------------------------------------------------- lenses and prisms

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

constructFrom :: Prism outer inner -> inner -> outer
constructFrom prism inner = fst (unPrism prism) inner

modifyWhen :: Prism outer inner -> (inner -> inner) -> (outer -> outer)
modifyWhen prism f outer = maybe outer (constructFrom prism . f) (getWhen prism outer)

-- (the counterpart, `field`, is just re-exported as-is)
constructor :: forall name inner outer. GenericLens.AsConstructor name inner outer => Prism outer inner
constructor = GenericLens._Ctor @name

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
  right' (Market bt seta) = Market (Right . bt) $ \cs -> case cs of
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



-------------------------------------------------------------------------- state monad

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

setState :: MonadState s m => s -> m ()
setState = State.put

modifyState :: MonadState s m => (s -> s) -> m ()
modifyState = State.modify'

setM :: MonadState outer m => Lens outer inner -> inner -> m ()
setM lens inner = modifyState (set lens inner)

doSetM :: MonadState outer m => Lens outer inner -> m inner -> m ()
doSetM lens action = do
    inner <- action
    setM lens inner

getM :: MonadState outer m => Lens outer inner -> m inner
getM lens = liftM (get lens) getState

modifyM :: MonadState outer m => Lens outer inner -> (inner -> inner) -> m ()
modifyM lens f = modifyState (modify lens f)

doModifyM :: MonadState outer m => Lens outer inner -> (inner -> m inner) -> m ()
doModifyM lens modifyAction = do
    oldInner <- getM lens
    newInner <- modifyAction oldInner
    setM lens newInner

getWhenM :: MonadState outer m => Prism outer inner -> m (Maybe inner)
getWhenM prism = liftM (getWhen prism) getState

constructFromM :: MonadState outer m => Prism outer inner -> inner -> m ()
constructFromM prism inner = setState (constructFrom prism inner)

modifyWhenM :: MonadState outer m => Prism outer inner -> (inner -> inner) -> m ()
modifyWhenM prism f = modifyState (modifyWhen prism f)

-- TODO `zoom` maybe?

(+=) :: (MonadState outer m, Num inner) => Lens outer inner -> inner -> m ()
lens += n = modifyM lens (+ n)

(-=) :: (MonadState outer m, Num inner) => Lens outer inner -> inner -> m ()
lens -= n = modifyM lens (subtract n)

(*=) :: (MonadState outer m, Num inner) => Lens outer inner -> inner -> m ()
lens *= n = modifyM lens (* n)

(/=) :: (MonadState outer m, Fractional inner) => Lens outer inner -> inner -> m ()
lens /= n = modifyM lens (/ n)

infixr 4 +=, -=, *=, /=






-------------------------------------------------------------------------- Text stuff

type LazyText = LazyText.Text

showText :: Show a => a -> Text
showText = Text.pack . show

prettyShow :: Show a => a -> Text
prettyShow = LazyText.toStrict . Text.Pretty.Simple.pShowLightBg

prettyPrint :: Show a => a -> IO ()
prettyPrint = Text.Pretty.Simple.pPrintLightBg



-------------------------------------------------------------------------- asserts

todo :: HasCallStack => a
todo = error "TODO"

bug :: HasCallStack => Text -> a
bug x = error (Text.unpack ("BUG: " ++ x))

class Assert x where
    type AssertResult x
    msgAssert :: HasCallStack => Text -> x -> AssertResult x

assert :: (HasCallStack, Assert x) => x -> AssertResult x
assert = msgAssert ""

assertM :: (HasCallStack, Assert x, Monad m) => x -> m (AssertResult x)
assertM x = return $! assert x

msgAssertM :: (HasCallStack, Assert x, Monad m) => Text -> x -> m (AssertResult x)
msgAssertM msg x = return $! msgAssert msg x

instance Assert Bool where
    type AssertResult Bool = ()
    msgAssert msg = bool (bug ("Failed assertion! " ++ msg)) ()

instance Assert (Maybe a) where
    type AssertResult (Maybe a) = a
    msgAssert msg = fromMaybe (bug ("Failed assertion! " ++ msg))

{- remove the Show constraint if it turns out to be problematic -}
instance Show e => Assert (Either e a) where
    type AssertResult (Either e a) = a
    msgAssert msg = fromRightOr (\e -> bug ("Failed assertion! " ++ msg ++ " " ++ showText e))



-------------------------------------------------------------------------- widely applicable project-specific definitions

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
    = ArithmeticOperator !ArithmeticOperator
    | ComparisonOperator !ComparisonOperator
    | LogicalOperator    !LogicalOperator
    deriving (Generic, Eq, Show)

instance Enumerable BinaryOperator where
    enumerate = map ArithmeticOperator enumerate ++ map ComparisonOperator enumerate ++ map LogicalOperator enumerate

data UnaryOperator
    = Not
    | Negate
    deriving (Generic, Eq, Show, Enum, Bounded)

instance Enumerable UnaryOperator
