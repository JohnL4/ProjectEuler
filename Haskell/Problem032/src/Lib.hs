module Lib
    ( sumPandigitalProducts, panDigitalTriples
    ) where

import Data.Map.Strict as Map (empty, toList, insertWith)

maxSqrt :: Integer
maxSqrt = ceiling $ sqrt 1e9

sumPandigitalProducts =
  sum $
  map
  (\(_,_,z) -> z)  -- Stupid simple function that takes a triple and returns the 3rd element.  The cute trick here is
                   -- that, since we don't care about the first two elements of the triple, we use '_' to specify them,
                   -- which means we don't care even enough to name the value, so long as SOMETHING is there.
  panDigitalTriples

panDigitalTriples = [(x, y, x*y)
                    | x <- candidateMultiplicandsInRange 1 maxSqrt,
                      y <- candidateMultiplicandsInRange (floor (1e8/x)) (ceiling (1e9/x)),
                      panDigital $ (show x) ++ (show y) ++ (show (x*y))
                    ]

-- | A list of all the candidate multiplicands between 'low' and 'high': all numbers with neither repeating digits nor
-- zeros.
candidateMultiplicandsInRange low high =
  [1..10]                       -- TODO

-- | Return true iff x has no repeated digits AND no zeros.
noRepeatedDigitsOrZeros s =
  (foldr (&&) True $ map (1 ==) $ map snd $ freqDist s)
  &&
  (not $ elem '0' $ map fst $ freqDist s)

-- | Return true iff the given string is pandigital (has each of the digits 1-9 exactly once).
panDigital s =
  noRepeatedDigitsOrZeros s
  &&
  length s == 9

-- | Frequency distribution of digits in single number s in form [(d,n)...], where d is digit in range [0..9] and n is
-- count of occurrence of digit.
freqDist :: String -> [(Char,Integer)]
freqDist s =
  Map.toList $
  foldr (\ digit map -> Map.insertWith (+) digit 1 map) Map.empty $
  s
