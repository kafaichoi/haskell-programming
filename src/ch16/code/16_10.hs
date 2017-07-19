import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a

data Pair a = Pair a a

data Two a b = Two a b

data Three a b c = Three a b c

data Three' a b = Three' a b b

data Four a b c d = Four a b c d

data Four' a b = Four' a a a b

instance Functor Identity where
        fmap f (Identity a) = Identity (f a)

instance Functor Pair where
        fmap f (Pair a b) = Pair (f a) (f b)

instance Functor (Two a) where
        fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
        fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
        fmap f (Three' a b c) = Three' a (f b) (f c)

instance Functor (Four a b c) where
        fmap f (Four a b c d) = Four a b c (f d)

instance Functor (Four' a) where
        fmap f (Four' a b c d) = Four' a b c (f d)


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToString = Fun Int String
type StringToInt = Fun String Int

main = do
        quickCheck $ \x -> functorIdentity (x :: [Int])
        quickCheck (functorIdentity :: [Int] -> Bool)
        quickCheck (functorCompose :: [Int] -> IntToString -> StringToInt -> Bool)

