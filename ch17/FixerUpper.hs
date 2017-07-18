module FixerUpper where

-- ex1
ex1 :: Maybe String
ex1 = const <$> Just "Hello" <*> pure "World"
-- ex2
ex2 :: Maybe (Integer, Integer, String, [Integer])
ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]
