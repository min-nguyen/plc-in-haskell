-- Examples of handwritten parsers.

module Handwritten where

-- base
import Data.Char

-- | A parser is a function from String to some value and remaining String.
type Parser a = String -> Maybe (a, String)

-- | A parser of ints.
number :: Parser Int
number (c:s)
  | isDigit c = Just (digits 0 (c:s))
number _      = Nothing

digits :: Int -> String -> (Int,String)
digits n (c:s)
  | isDigit c = digits (10*n + digitToInt c) s
digits n s    = (n,s)