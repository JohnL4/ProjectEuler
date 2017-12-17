module Lib
    ( sumPandigitalProducts, panDigitalTriples
    ) where

import Data.Map.Strict as Map (empty, toList, insertWith)
import Data.List

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
                      y <- candidateMultiplicandsInRange (floor (1e8/(fromIntegral x))) (ceiling (1e9/(fromIntegral x))),
                      panDigital $ (show x) ++ (show y) ++ (show (x*y))
                    ]

-- | A list of all the candidate multiplicands between 'low' and 'high': all numbers with neither repeating digits nor
-- zeros.
candidateMultiplicandsInRange low high =
    takeWhile (< high) $ candidatesGreaterThanOrEqualTo low [1..9]
    -- [1..10]                       -- TODO

-- | List of numbers with unique digits greater than or equal to 'low', built using digits from 'availableDigits'
candidatesGreaterThanOrEqualTo :: Integer -> [Integer] -> [Integer]
candidatesGreaterThanOrEqualTo _ [] = []
candidatesGreaterThanOrEqualTo
  low
  availableDigits               -- ^ Sorted (ascending) list of available digits in range [1..9]
{-
32415 [1..9] --> itself, 3241[6..9], cands 32450 [1,6..9], cands 32460 [1,5,7,8,9]
-}
  | low < 10 = dropWhile (< low) availableDigits
  | otherwise =
    let n = floor $ logBase 10 $ fromIntegral low
        firstDigit = low `quot` 10^n
        nextAvailable = dropWhile (< firstDigit) availableDigits -- Candidates for use in the current ("next") digit
    in
      []

-- | Utility to return the digits of n as a sequence (most-significant digit first)
numToDigits :: Integer -> [Integer]
numToDigits n
  | n < 10 = [n]
  | otherwise = f:(numToDigits $ n-f*10^d)
  where
    d = floor $ logBase 10 $ fromIntegral n
    f = n `quot` (10^d)

-- | Utility to return the number represented by the sequence of digits (most-significant digit first)
digitsToNum digits
  = foldl' (\t d -> 10*t + d) 0 digits -- Note foldl' instead of foldr, because we want to evaluate for the leftmost
    -- digit first (foldr will evaluate for the rightmost digit first, using right-associative application of the
    -- function given).

numsStartingWith n allowed
  =
  map digitsToNum
  $ seqStartingWith (numToDigits n) allowed

-- | Returns the smallest number consisting of unique digits greater than or equal to 'n'
smallestUniqueGE n
  =  digitsToNum $ smallestUniqueSeqGE (numToDigits n) [1..9]

-- | Returns the smallest sequence consisting of unique digits greater than or equal to its first argument, and whose
-- digits from the list of allowed digits.
smallestUniqueSeqGE [] _ = []
smallestUniqueSeqGE (digit:digits) allowed
  = allowedDigit : smallestUniqueSeqGE digits (allowed \\ [allowedDigit])
  where
    allowedDigit = head $ dropWhile (\d -> not $ elem d allowed) [digit..9]

-- | Sequences of digits starting w/the lowest unique one above 'seed' and continuing upward but having the same length
-- as 'initial'
seqStartingWith :: [Integer] -> [Integer] -> [[Integer]]
seqStartingWith [d] allowed = map (\x -> [x]) $ dropWhile (<d) allowed
seqStartingWith
  initial                       -- Initial "seed"
  allowed                       -- Allowed digits
  =
  (map (head initial :) $ seqStartingWith (tail initial) (allowed \\ ([head initial])))
  ++
  (flatten1
   $ map (\d -> map (d :)
           $ seqStartingWith
           (take ((length initial) - 1) (repeat 1))
           (allowed \\ ([d])))
    (dropWhile (<= (head initial)) allowed))

-- | Utility to flatten a list of lists exactly one level
flatten1 :: [[a]] -> [a]
flatten1 as = foldr (++) [] as

{-
-- | Return a sequence consisting of the head of the input list followed by zeros, having the same length as the
-- original sequence.
zeroRest :: Num a => [a] -> [a]
zeroRest [] = []
zeroRest (a:as) = a : take (length as) (repeat 0)
-}

-- | Return a sequence of sequences having the same length as the input sequence but corresponding to numbers greater
-- than or equal to the number represented by the input sequence
-- seqSameLengthFromArbitrary digs = seqSameLengthFromUnique $ unique digs $ [1..9] \\ TODO





  



{-
  =
  (candidatesWithSameLeadingDigitButGreaterThanOrEqualTo low availableDigits)
  ++
  (candidatesWithLargerLeadingDigits low availableDigits)
-}



candidatesWithSameLeadingDigitButGreaterThanOrEqualTo
  low
  availableDigits
  | low < 10 = dropWhile (< low) availableDigits
  | otherwise =
    let n = floor $ logBase 10 $ fromIntegral low
        firstDigit = low `quot` 10^n
        nextAvailable = head $ dropWhile (< firstDigit) availableDigits -- Candidates for use in the current ("next") digit
    in
      map (nextAvailable * 10^n +) $
      candidatesGreaterThanOrEqualTo
      (low - firstDigit * 10^n) 
      (fst $ break (==nextAvailable) availableDigits) ++ (tail $ snd $ break (==nextAvailable) availableDigits)



candidatesWithLargerLeadingDigits low availableDigits 
  | low < 10 = dropWhile (< low) availableDigits
  | otherwise =
    let n = floor $ logBase 10 $ fromIntegral low
        firstDigit = low `quot` 10^n
        nextAvailable = dropWhile (<= firstDigit) availableDigits
    in
      foldr (++) [] $             -- Fold a list of lists into a single list
      map                         -- Build a list of lists of numbers starting with all digits that give us a value >=
                                  -- the current value, which means the first digit must be >= the first digit of the
                                  -- current (input) number
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
