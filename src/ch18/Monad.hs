module Monad where

import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

-- Either Monad
data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First f <*> _ = First f
  Second f <*> First x = First x
  Second f <*> Second x = Second $ f x

instance Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b
