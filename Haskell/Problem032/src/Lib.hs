module Lib
    ( sumPandigitalProducts, panDigitalTriples
    ) where

import Data.Map.Strict as Map (empty, toList, insertWith)

maxSqrt :: Integer
maxSqrt = ceiling $ sqrt 1e9

sumPandigitalProducts =
  sum $
  Prelude.map (\(_,_,z) -> z) panDigitalTriples

panDigitalTriples = [(x, y, x*y)
                    | x <- [1..maxSqrt], noRepeatedDigitsOrZeros $ show x,
                      y <- [100000000 `quot` x..1000000000 `quot` x], noRepeatedDigitsOrZeros $ show y,
                      panDigital $ (show x) ++ (show y) ++ (show (x*y))
                    ]

-- | Return true iff x has no repeated digits AND no zeros.
noRepeatedDigitsOrZeros s =
  (foldr (&&) True $ map (1 ==) $ Prelude.map snd $ freqDist s)
  &&
  (not $ elem '0' $ map fst $ freqDist s)

panDigital s =
  noRepeatedDigitsOrZeros s
  &&
  length s == 9

-- | Frequency distribution of digits in single number s in form [(d,n)...], where d is digit in range [0..9] and n is
-- count of occurrence of digit.
freqDist :: String -> [(Char,Integer)]
freqDist s =
  Map.toList $
  Prelude.foldr (\ digit map -> Map.insertWith (+) digit 1 map) Map.empty $
  s
