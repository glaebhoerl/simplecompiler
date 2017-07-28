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

data ArithmeticOperator = Add | Sub | Mul | Div | Mod deriving (Eq, Show)

data ComparisonOperator = Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual deriving (Eq, Show)

data LogicalOperator = And | Or deriving (Eq, Show)

data BinaryOperator = ArithmeticOperator !ArithmeticOperator | ComparisonOperator !ComparisonOperator | LogicalOperator !LogicalOperator deriving (Eq, Show)

data UnaryOperator = Not | Negate deriving (Eq, Show)
