module Lib
    ( pythagoreanTripletSummingTo
    ) where

pythagoreanTripletSummingTo :: Integer -> [Integer]
pythagoreanTripletSummingTo n =
  head [ [a,b,c] | a <- [1..n-4], b <- [a+1..n-3], c <- [b+1..n-2], -- max. c can only be n-2, since a & b must be at least 1.
         a^2 + b^2 == c^2,
         a + b + c == n]
