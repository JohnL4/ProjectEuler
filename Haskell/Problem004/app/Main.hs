module Main where

import Lib
import Text.Printf

main :: IO ()
main = do
  printf "largest square palindrome: %d\n" (121::Integer)

-- Using Maxima (pgm), prove to yourself that x * (x+y) - (x-1) * (x+y+1) is always positive:
-- expand(x*(x+y)-(x-1)*(x+y+1)) --> y + 1
-- e.g., 995 * 998 - 994 * 999 = 4
-- So, the diagonal on the 2x2 matrix of products is the "ridge" (maxima), and the farther off the "ridge" you go, the
-- farther from the "maximum" you fall.  Not sure how useful that is.

-- If you have found some palindrome p and you are considering pairs of numbers (x1,x2) (where x1 >= x2) in constructing
-- a larger palindrome, you don't need to consider x2's smaller than p/x1, since x1 * p/x1 = p.

largestSquarePalindrome = maximum [ p^2 | p <- [999,998..1], isPalindrome (p^2) ] -- 698896

multiples = 1

isPalindrome x =
  leftHalf x == (reverse $ rightHalf x)

leftHalf x =
  take halfLength (show x)
  where
    halfLength = quot (length $ show x) 2

rightHalf x =
  drop (halfLength + (if (n `mod` 2) == 1 then 1 else 0)) (show x)
  where
    n = length $ show x
    halfLength = n `quot` 2

