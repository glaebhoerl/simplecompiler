module MyPrelude (module MyPrelude, module Reexports) where

import Prelude             as Reexports hiding (foldl)
import Control.Applicative as Reexports
import Data.Foldable       as Reexports (foldl')
import Data.Text           as Reexports (Text)

todo :: a
todo = error "TODO"

liftA0 :: Applicative f => a -> f a
liftA0 = pure

liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 = fmap

oneOf :: Alternative f => [f a] -> f a
oneOf = foldl' (<|>) empty

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
