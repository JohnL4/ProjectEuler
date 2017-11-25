module Main where

import Lib
import Text.Printf

main :: IO ()
main = do
  printf "largest square palindrome: %d\n" largestSquarePalindrome
  printf "largest palindrome:        %d\n" largestPalindrome

-- Using Maxima (pgm), prove to yourself that x * (x+y) - (x-1) * (x+y+1) is always positive:
-- 
-- expand(x*(x+y)-(x-1)*(x+y+1)) --> y + 1
-- 
-- e.g., 995 * 998 - 994 * 999 = 4
-- 
-- So, the diagonal on the 2x2 matrix of products is the "ridge" (maxima), and the farther off the "ridge" you go, the
-- farther from the "maximum" you fall.  Not sure how useful that is.

-- If you have found some palindrome p and you are considering pairs of numbers (x1,x2) (where x1 >= x2) in constructing
-- a larger palindrome, you don't need to consider x2's smaller than p/x1, since x1 * p/x1 = p.

largestSquarePalindrome = maximum [ p^2 | p <- ([999,998..1]::[Integer]), isPalindrome (p^2) ] -- 698896, or 836^2

-- After above, define d3(n,m) := (m+836)*(n+836)−698896 (d3 was the third delta function I defined).
--
-- Solve d3(n,163) = 0 for n and you get -136.  The 163 comes from 999-836 = 163.  So, somewhere in the triangle defined
-- by (836,836), (700,999), and (999,999) is the greatest palindrome.  All numbers outside this triangle are less than
-- 836^2.
--
-- Actually, it's NOT a triangle because the edge from (836,836) to (700,999) isn't straight (why not? is it
-- hyperbolic?).  So, I guess we should consider 700 to be our floor and cover all numbers above it.

largestPalindrome = maximum $ [ (fst p) * (snd p) -- 'fst' is first of a pair; 'snd', second.
                              -- A list that is the Cartesian product of two other lists.  Sort of.  It's really a
                              -- "triangular" Cartesian product.  Also note that I had to specify the type of one of the
                              -- constants ('[Integer]' means "list of Integers"), since Haskell can't guess them.
                              | p <- [ (x,y) | x <- ([700..999]::[Integer]), y <- [x..999] ], 
                                isPalindrome $ (fst p) * (snd p) ]
                                

isPalindrome x =
  leftHalf x == (reverse $ rightHalf x)

leftHalf x =
  take halfLength (show x)
  where
    halfLength = quot (length $ show x) 2 -- 'quot' is integer division (quotient).

rightHalf x =
  drop (halfLength + (if (n `mod` 2) == 1 then 1 else 0)) (show x)
  where
    n = length $ show x
    halfLength = n `quot` 2

