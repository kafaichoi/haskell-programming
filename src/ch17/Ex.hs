module Ex where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Pair
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair a a' = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Pair a a')

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- Two
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two a' b = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return (Two a a')

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- Four
data Four a b c d =
  Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a b c f <*> Four a' b' c' x =
    Four (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
    => Arbitrary (Four a b c d) where
  arbitrary = genFour Four

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- Four'

data Four' a b =
  Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a b c f <*> Four' a' b' c' x =
    Four' (a <> a') (b <> b') (c <> c') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour Four'

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

genFour f = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ f a b c d


type S = String
type I = Int

main :: IO ()
main = do
  putStr "\n -- Pair"
  quickBatch (applicative (undefined :: Pair (S, S, S)))
  putStr "\n -- Two"
  quickBatch (applicative (undefined :: Two (S, S, S) (I, I, I)))
  putStr "\n -- Four"
  quickBatch (applicative (
    undefined :: Four (S, S, S) (S, S, S) (S, S, S) (I, I, I)))
  putStr "\n -- Four'"
  quickBatch (applicative (
    undefined :: Four' (S, S, S) (I, I, I)))
