module Validation where

import           Control.Applicative
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Validation Applicative

data Validation e a =
  Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  Success' f <*> Success' a = Success' (f a)
  Success' _ <*> Failure' e = Failure' e
  Failure' e <*> Success' _ = Failure' e
  Failure' e1 <*> Failure' e2 = Failure' $ e1 <> e2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure' e, Success' a]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = quickBatch (applicative (undefined :: Validation String (String, String, String)))
