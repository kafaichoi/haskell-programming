module Caeser where

import Data.Char

shift :: Int -> Char -> Char
shift n = chr . (+n) . ord

caeser :: Int -> String -> String
caeser n = map $ shift n

uncaeser :: Int -> String -> String
uncaeser n = caeser (-n)
