module Main where

import Lib
import Text.Printf
import Data.Numbers.Primes
import Data.Time.Clock.POSIX

n = 20::Integer

main :: IO ()
main = do
  solveStart <- getPOSIXTime
  printf "smallest number divisible all integers in range [2..%d]: %d" n (smallestBy n)

  -- So, note: getPOSIXTime is not a function, it's an action.  The "<-" is not an assignment, but is instead a binding
  -- to the result of the action, whenever it's invoked.  (It's a lot like an assignment, though.)  So, since it's not a
  -- function, you can't just throw it in an expression where a function might occur (like, say "getPOSIXTime -
  -- solveStart").  In Haskell actions (e.g., "main"), you're sort of forced to use "assignment" statements like the
  -- following to capture the results of an action like getPOXISTime.
  
  solveStop <- getPOSIXTime
  printf "\t(%s sec)\n" $ show (solveStop - solveStart)
  printf "...and, oh, by the way, a couple of primes:\n"
  millionStart <- getPOSIXTime
  printf "\tmillionth:\t%d" ((primes::[Integer])!!999999)
  millionStop <- getPOSIXTime
  printf "\t(%s sec)\n" $ show (millionStop - millionStart)
  tenMillionStart <- getPOSIXTime
  printf "\tTEN millionth:\t%d" ((primes::[Integer])!!9999999)
  tenMillionStop <- getPOSIXTime
  printf "\t(%s sec)\n" $ show (tenMillionStop - tenMillionStart)

-- Well, I was really proud of this more-or-less brute-force solution until I saw the first comment by a solver in 2004.
-- Turns out you can do this with some simple prime factorization and you don't need any programming at all.
-- I'm chagrined.

smallestBy n =
  head $ dropWhile (\x -> not $ evenlyDivisibleByAll n x) [n..]

evenlyDivisibleByAll n x =
  null $ dropWhile (== 0) $ map (mod x) [n,n-1..2]
