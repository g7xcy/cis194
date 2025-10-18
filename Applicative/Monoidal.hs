module Monoidal where

class Applicative f => Monoidal f where
  unit :: f ()
  unit = pure ()

  (<**>) :: f a -> f b -> f (a, b)
  (<**>) = liftA2 (,)

class Monoidal f => Applicative' f where
  pure' :: a -> f a
  pure' x = fmap (const x) unit

  (<*/>) :: f (a -> b) -> f a -> f b
  -- (<**>) f x => f (a -> b, a)
  -- ($) :: (a -> b) -> a -> b
  -- uncurry :: (a -> b -> c) -> (a, b) -> c
  (<*/>) f x =  fmap (uncurry ($)) ((<**>) f x)

