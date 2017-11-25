module Main where

import Lib
import Text.Printf

n = 100::Integer

main :: IO ()
main = do
  printf "Square of sum minus sum of squares = %d\n" (squareSum n - sumSquares n)
