module Main where

import Lib
import Text.Printf

n = 13

main :: IO ()
main = do
  printf "Greatest product of subsequence of length %d: %d\n" n (greatestProductSeq n)

