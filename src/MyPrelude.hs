module MyPrelude where

todo :: a
todo = error "TODO"

liftA0 :: Applicative f => a -> f a
liftA0 = pure

liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 = fmap
