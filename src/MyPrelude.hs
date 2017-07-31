module MyPrelude (module MyPrelude, module Reexports) where

import Prelude             as Reexports hiding (foldl, (++), head)
import Control.Applicative as Reexports
import Data.Foldable       as Reexports (foldl')
import Data.Text           as Reexports (Text)

todo :: a
todo = error "TODO I should use HasCallStack here"

head :: [a] -> Maybe a
head = \case
    []  -> Nothing
    a:_ -> Just a

single :: a -> [a]
single = \a -> [a]

liftA0 :: Applicative f => a -> f a
liftA0 = pure

liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 = fmap

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl' (<|>) empty

zeroOrOne :: Alternative f => f a -> f [a]
zeroOrOne a = liftA0 [] <|> liftA1 single a

oneOrMore :: Alternative f => f a -> f [a]
oneOrMore = some

zeroOrMore :: Alternative f => f a -> f [a]
zeroOrMore = many

(++) :: Monoid a => a -> a -> a
(++) = mappend

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
    deriving (Eq, Show, Enum, Bounded, Enumerable)

data ComparisonOperator
    = Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Equal
    | NotEqual
    deriving (Eq, Show, Enum, Bounded, Enumerable)

data LogicalOperator
    = And
    | Or
    deriving (Eq, Show, Enum, Bounded, Enumerable)

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
    deriving (Eq, Show, Enum, Bounded, Enumerable)
