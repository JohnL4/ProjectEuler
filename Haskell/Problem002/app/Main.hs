module Main where

import Lib

main :: IO ()
main = do
  print fibs
  print $ sum fibs
  where
    fibs = takeWhile (< 4000000)
      $ filter (\x -> x `mod` 2 == 0) -- Lambda function, plus a 2-arg function turned into a binary operator with backtics.
      $ fibonaccis 
      -- [1..10]

-- See https://wiki.haskell.org/The_Fibonacci_sequence#Canonical_zipWith_implementation
fibonaccis = 0 : 1 : zipWith (+) fibonaccis (tail fibonaccis) 
