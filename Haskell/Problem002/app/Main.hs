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
-- 
-- I figured out how this works.  It's recursion, but not the normal kind.  Normally, a recursive algorithm makes a
-- problem smaller with each recursive step until it finally terminates.  This, instead, is infinite recursion, because
-- it doesn't make the problem smaller with each step and it never gets to a base terminating case.  In a normal
-- (non-lazy, imperative) language, this would go into infinite recursion, overflow the stack space and bomb the
-- program.
--
-- The reason it works in this case is that Haskell is lazy -- it only evaluates what it needs to, and no more.  If we
-- ask for three numbers from the Fibonacci sequence, we get the 0 and the 1 and then we get exactly one step of the
-- 'zipWith' function evaluated, which adds 0 (the first element of 'fibonaccis') and 1 (the first element of 'tail
-- fibonaccis') to give 1, and then evaluation stops.
fibonaccis = 0 : 1 : zipWith (+) fibonaccis (tail fibonaccis) 
