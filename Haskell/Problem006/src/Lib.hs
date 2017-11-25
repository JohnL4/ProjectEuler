module Lib
    ( squareSum, sumSquares
    ) where

squareSum :: Integer -> Integer
squareSum n =
  (sum [1..n])^2

sumSquares :: Integer -> Integer
sumSquares n =
  foldr (+) 0 $ map (^2) [1..n]
