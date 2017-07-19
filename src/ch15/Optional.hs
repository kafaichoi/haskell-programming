module Optional where

import Data.Monoid
import Test.QuickCheck

data Optional a
  = Some a
  | None

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = arbitrary >>= \a -> elements [return (Some a), return None]

instance Monoid a => Monoid (Optional a) where
  mempty = None
  mappend (Some x) (Some y) = Some (x <> y)
  mappend (Some x) _ = Some x
  mappend _ (Some x) = Some x
  mappend _ _ = None

  
  
