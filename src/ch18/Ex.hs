module Ex where

import           Control.Applicative
import           Control.Monad            (join, (>=>))
import           Data.Functor
import           Data.Monoid
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Nope

data Nope a =
  NopeDotJpg

instance Functor Nope where
 fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

type S = String
type I = Int

main :: IO ()
main = do
  putStr "\n -- Nope"
  quickBatch $ monad  (undefined :: Nope (I, I, I))
