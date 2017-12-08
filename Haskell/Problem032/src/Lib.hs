module Lib
    ( sumPandigitalProducts
    ) where

import Data.Map.Strict as Map

sumPandigitalProducts = 0

noRepeatedDigits x = False

-- | Frequency distribution of digits in single number x in form [(d,n)...], where d is digit in range [0..9] and n is
-- count of occurrence of digit.  TODO: be sure to eliminate 0s from consideration
freqDist :: Integer -> [(Integer,Integer)]
freqDist1 x =
  Map.toList $
  foldr (\ digit map -> Map.insertWith (1+) digit 1) Map.empty $
  show x
