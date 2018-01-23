module Lib
    ( sumPandigitalProducts, panDigitalTriples
      -- unit test:
    , numToDigits, digitsToNum, smallestUniqueGE, flatten1, candidatesGreaterThanOrEqualTo
    ) where

import Data.Map.Strict as Map (empty, toList, insertWith)
import Data.List
import Debug.Trace

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
    takeWhile (< high) $ candidatesGreaterThanOrEqualTo low
    -- [1..10]                       -- TODO

-- | List of numbers with unique digits greater than or equal to 'low', built using digits from 'availableDigits'
-- candidatesGreaterThanOrEqualTo :: Integer -> Maybe [Integer]
candidatesGreaterThanOrEqualTo low | trace ("candidatesGreaterThanOrEqualTo " ++ show low) False = undefined
candidatesGreaterThanOrEqualTo low =
  case smallestUniqueGE low of
    Just smallest -> smallest : candidatesGreaterThanOrEqualTo (smallest + 1)
    Nothing -> []

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

{-
numsStartingWith n allowed
  =
  map digitsToNum
  $ seqStartingWith (numToDigits n) allowed
-}

-- | Returns the smallest number consisting of unique digits greater than or equal to 'n'
smallestUniqueGE :: Integer -> Maybe Integer
smallestUniqueGE n =
  if n < 987654321
  then case smallestSeq of
    Nothing ->
      if (length $ numToDigits n) < 9
      then {- add an extra digit -} Just $ digitsToNum $ take ((length $ numToDigits n) + 1) [1..9]
      else Nothing
    Just s  -> Just (digitsToNum s)
  else Nothing
  where smallestSeq = smallestUniqueSeqGE (numToDigits n) ([1..9] \\ [head $ numToDigits n])
{-
  if (n > smallest)
  then smallestUniqueGE (n+1)
  else smallest
  where smallest = digitsToNum $ smallestUniqueSeqGE (numToDigits n) [1..9]
-}
  
-- | Returns the smallest sequence consisting of unique digits greater than or equal to its first argument, and whose
-- digits are from the list of allowed digits.  If there is no such sequence of the same length as the input sequence,
-- will return Nothing.
smallestUniqueSeqGE ::
  [Integer]                     -- ^ "Seed" sequence
  -> [Integer]                  -- ^ List of digits that are allowed to comprise the return value (note that the first
                                -- digit is automatically allow)
  -> Maybe [Integer]            -- ^ Nothing ==> couldn't find a number meeting the requirements

smallestUniqueSeqGE (digit:[]) allowed | trace ("smallestUniqueSeqGE (" ++ show digit ++ ":[]) " ++ show allowed) False = undefined
smallestUniqueSeqGE (digit:[]) allowed =
  if 0 == (length $ dropWhile (< digit) allowed)
  then Nothing
  else Just [head $ dropWhile (< digit) allowed]

smallestUniqueSeqGE a b | trace ("smallestUniqueSeqGE " ++ show a ++ " " ++ show b) False = undefined
smallestUniqueSeqGE (digit:digits) allowed =
  if (noRepeatedDigitsOrZeros (digit:digits) 0)
  then Just (digit:digits)
  else
    if null subseqs
    then Nothing
    else case (head subseqs) of
      Nothing -> Nothing          -- Should never happen, since we should have already filtered all the Nothings out,
                                  -- below. 
      Just s  -> Just (digit : s)
    where
      subseqs =                   -- ^ List of all possible sequences starting with digits (Note: 8:[8,8] -> [9,1])
        filter (/= Nothing) $
        -- This isn't right. We should check to see if current subseq is unique, and, if so, return it.  Otherwise, return
        -- take n allowed where n is the length of current subseq (or some such -- need to make sure first digit is bigger
        -- than current first digit).
        map (\nextDigit -> smallestUniqueSeqGE (nextDigit : tail digits) (allowed \\ [digit])

                       )
                -- Candidate next digits are all those >= allowed digits less the leading digit.
                $ dropWhile (< head digits) $ allowed \\ [digit]
  


{-
                if elem $ head digits $ allowed \\ digit
  then case subseq of
    Just ds -> digit : subseq 
  where
    subseq = 
-}

{-
  if (null allowedDigits || length subseq > length digits || head subseq < head digits)
  -- Unexpected length ==> had to carry a one ==> can't just tack on computed "next" to existing digits ==> just
  -- compute a new "next".
  -- then take ((length (digit:digits))+1) [1..9]
  then smallestUniqueSeqGE (numToDigits (digitsToNum (digit:digits) + 1)) allowed
  else head allowedDigits : subseq
  where
    allowedDigits = dropWhile (\d -> not $ elem d allowed) [digit..9]
    subseq = smallestUniqueSeqGE digits (allowed \\ [head allowedDigits])
-}
      
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


{-
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
-}

-- | Return true iff sequence has no repeated digits AND no zeros (i.e., no element equal to 'disallowed').
-- noRepeatedDigitsOrZeros :: [Integer] -> Boolean
noRepeatedDigitsOrZeros
  s                             -- Sequence to be checked
  disallowed                    -- The element we're not allowing, either 0 or '0'.
  =
  (foldr (&&) True $ map (1 ==) $ map snd $ freqDist s)
  &&
  (not $ elem disallowed $ map fst $ freqDist s)

-- | Return true iff the given string is pandigital (has each of the digits 1-9 exactly once).
panDigital s =
  noRepeatedDigitsOrZeros s '0'
  &&
  length s == 9

-- | Frequency distribution of digits in single number s in form [(d,n)...], where d is digit in range [0..9] and n is
-- count of occurrence of digit.
-- freqDist :: String -> [(Char,Integer)]
freqDist s =
  Map.toList $
  foldr (\ digit map -> Map.insertWith (+) digit 1 map) Map.empty $
  s
