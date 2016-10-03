data TisAnInteger =
  TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) ( TisAn y) = x == y

data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x1 x2) (Two y1 y2) = x1 == y1 && x2 == y2

data StringOrInt =
  TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x == y
  (==) (TisAString x) (TisAString y) = x == y
  (==) _ _ = False

data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a1 b1) (Pair a2 b2) = a1 == a2 && b1 == b2

data Tuple a b =
  Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2

data Which a =
  ThisOne a
	| ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _                     = False

data EitherOr a b =
	Hello a
	| Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False
