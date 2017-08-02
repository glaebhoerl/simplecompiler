module MyPrelude (module MyPrelude, module Reexports) where

import Prelude                   as Reexports hiding (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile, head, tail, (++), foldl)
import Data.Text.IO              as Reexports        (putStr, putStrLn, getLine, getContents, interact, readFile, writeFile, appendFile)
import Data.Text                 as Reexports        (Text)
import Data.Foldable             as Reexports        (foldl')
import Control.Applicative       as Reexports -- TODO write explicit import lists for these too
import Control.Monad             as Reexports
import Control.Monad.Trans.Class as Reexports
import Data.Either               as Reexports
import Data.Maybe                as Reexports

import qualified Data.Text as Text

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

prepend :: a -> [a] -> [a]
prepend = (:)

single :: a -> [a]
single = \a -> [a]

left :: Either a b -> Maybe a
left = either Just (const Nothing)

right :: Either a b -> Maybe b
right = either (const Nothing) Just

bool :: a -> a -> Bool -> a
bool false true b = if b then true else false

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

showText :: Show a => a -> Text
showText = Text.pack . show

todo :: a
todo = error "TODO I should use HasCallStack here"

bug :: Text -> a
bug x = error (Text.unpack ("BUG: " ++ x))

-- TODO HasCallStack
class Assert x where
    type AssertResult x
    assert    :: x -> AssertResult x
    msgAssert :: Text -> x -> AssertResult x
    assert = msgAssert ""

assertM :: (Assert x, Monad m) => x -> m ()
assertM x = assert x `seq` return ()

msgAssertM :: (Assert x, Monad m) => Text -> x -> m ()
msgAssertM msg x = msgAssert msg x `seq` return ()

instance Assert Bool where
    type AssertResult Bool = ()
    msgAssert msg = bool (bug ("Failed assertion! " ++ msg)) ()

instance Assert (Maybe a) where
    type AssertResult (Maybe a) = a
    msgAssert msg = fromMaybe (bug ("Failed assertion! " ++ msg))

-- (remove the Show constraint if it turns out to be problematic)
instance Show e => Assert (Either e a) where
    type AssertResult (Either e a) = a
    msgAssert msg = either (\e -> bug ("Failed assertion! " ++ msg ++ " " ++ showText e)) id

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
    deriving (Eq, Show, Enum, Bounded)

instance Enumerable ArithmeticOperator

data ComparisonOperator
    = Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Equal
    | NotEqual
    deriving (Eq, Show, Enum, Bounded)

instance Enumerable ComparisonOperator

data LogicalOperator
    = And
    | Or
    deriving (Eq, Show, Enum, Bounded)

instance Enumerable LogicalOperator

data BinaryOperator
    = ArithmeticOperator !ArithmeticOperator
    | ComparisonOperator !ComparisonOperator
    | LogicalOperator    !LogicalOperator
    deriving (Eq, Show)

instance Enumerable BinaryOperator where
    enumerate = map ArithmeticOperator enumerate ++ map ComparisonOperator enumerate ++ map LogicalOperator enumerate

data UnaryOperator
    = Not
    | Negate
    deriving (Eq, Show, Enum, Bounded)

instance Enumerable UnaryOperator
