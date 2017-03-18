module Caeser where

import Data.Char

shift :: Int -> Char -> Char
shift n = chr . (+n) . ord

caeser :: Int -> String -> String
caeser n = map $ shift n

uncaeser :: Int -> String -> String
uncaeser n = caeser (-n)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny predicate = myOr . map predicate 

myElem :: Eq a => a -> [a] -> Bool
myElem a = myAny (== a)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) =
  let
    y = myMaximumBy f xs
  in
    case f x y of
      LT -> y
      EQ -> x
      GT -> x

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f =
  let
    f' a b = case f a b of
      LT -> GT
      EQ -> EQ
      GT -> LT
  in
    myMaximumBy f'

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

