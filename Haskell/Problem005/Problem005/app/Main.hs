module Main where

import Lib
import Text.Printf

n = 20::Integer

main :: IO ()
main =
  printf "smallest number divisible all integers in range [2..%d]: %d\n" n (smallestBy n)

-- Well, I was really proud of this more-or-less brute-force solution until I saw the first comment by a solver in 2004.
-- Turns out you can do this with some simple prime factorization and you don't need any programming at all.
-- I'm chagrined.

smallestBy n =
  head $ dropWhile (\x -> not $ evenlyDivisibleByAll n x) [n..]

evenlyDivisibleByAll n x =
  null $ dropWhile (== 0) $ map (mod x) [n,n-1..2]
