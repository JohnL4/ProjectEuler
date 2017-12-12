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
                      y <- candidateMultiplicandsInRange (fromIntegral $ floor (1e8/x)) (fromIntegral $ ceiling (1e9/x)),
                      panDigital $ (show x) ++ (show y) ++ (show (x*y))
                    ]

-- | A list of all the candidate multiplicands between 'low' and 'high': all numbers with neither repeating digits nor
-- zeros.
candidateMultiplicandsInRange low high =
    takeWhile (< high) $ candidatesGreaterThanOrEqualTo low [1..9]
    [1..10]                       -- TODO

-- | List of numbers with unique digits greater than or equal to 'low', built using digits from 'availableDigits'
candidatesGreaterThanOrEqualTo _ [] = []
candidatesGreaterThanOrEqualTo
  low
  availableDigits               -- ^ Sorted (ascending) list of available digits in range [1..9]
  =
  let n = floor $ logBase 10 low
      firstDigit = low `quot` 10^n
      nextAvailable = dropWhile (< firstDigit) availableDigits -- Candidates for use in the current ("next") digit
      bestAvailable = head $ dropWhile (< firstDigit) availableDigits
      rest = takeWhile (< bestAvailable) availableDigits ++ dropWhile (<= bestAvailable) availableDigits
  in
    map                         -- Build a list of numbers starting with all digits that give us a value >= the current
                                -- value, which means the first digit must be >= the first digit of the current (input)
                                -- number
    (\digit ->
       map (digit * 10^n +) $
       candidatesGreaterThanOrEqualTo
       (low - firstDigit * 10^n) 
       (fst $ break (==digit) availableDigits) ++ (tail $ snd $ break (==digit) availableDigits)
       )
    nextAvailable
    

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
