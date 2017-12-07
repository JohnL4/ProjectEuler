module Main where

import Lib
import Text.Printf
import Data.Time.Clock.POSIX

-- | The amount we're trying to find different ways of making, using different coin denominations.
amount :: Integer
amount = 11

main :: IO ()
main = do

  -- Print, for debugging and verification that we're on the right track.  If 'amount' is 200, this takes FOREVER
  -- (because it's building one giant string containing 73,000 something lines), so this has to be commented out.
  --
  -- What's super, super interesting about this is that when I ran this with 'amount' = 200, my CPU utilization went to
  -- 500% (how is that possible on a 4-core Mac Mini?), which means, I think, that Haskell automatically parallelized
  -- the computation.  That's one of the huge beauties of pure functional programming: since functions don't have side
  -- effects, you can split the work up and run a bunch of them in parallel.  In the case of building a string from
  -- 73,000 entries, you can chunk up the work into building (# of cores) equal-sized (roughly) substrings and glue them
  -- together at the end.

  printf "%s\n"                 -- Print a single string, which is coming up next.
    (foldr                      -- "fold": fold a list into one value (a single string, in this case), by applying a
                                --   function to an accumulator.
     (\ pile soFar ->           -- Here's the function, in our favorite lambda notation (you can read `\` as the Greek
                                --   letter lambda (λ)). it takes two arguments: the current entry from the list we're
                                --   folding (pile), and the accumulator (what we have "so far") and returns a value of
                                --   the same type as the 2nd argument (string, in this case).
        soFar                   -- Function body: accumulate by tacking more junk onto the end of 'soFar'.
        ++                      -- String concatenation
        (show pile)             -- 'show' is the magic turn-my-object-into-a-string-humans-can-read function that
                                --   Haskell built for us because we derived 'Show' in Lib.hs.  It's kind of like
                                --   .toString() in other languages.
        ++ "\n"                 -- Slapping a newline between each pile
     )                          -- Here endeth our lambda function
     ""                         -- 2nd argument to 'foldr': the initial value of our accumulator ('soFar', above)
     $                          -- I went over the intricacies of '$' in an earlier Euler problem, but basically it's
                                --   way to have fewer parentheses.
     changeCombinations amount) -- 3rd argument to 'foldr' is the result of calling the function 'changeCombinations'
                                --   with argument 'amount'
  
  startTime <- getPOSIXTime
  
  printf "%d ways of making %d" (length $ changeCombinations amount) amount

  stopTime <- getPOSIXTime
  printf "\t(%s sec)\n" $ show (stopTime - startTime)
