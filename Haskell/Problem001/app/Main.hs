module Main where

import Lib
import Data.List as List
import Data.Set as Set

main :: IO()
main = do
  print nums
  print $ sum nums              -- '$' is a precedence-0 do-nothing (identity) operator that basically means "evaluate
                                -- everything that comes after me before you continue to evaluate the expression I'm
                                -- in".  It's the equivalent of putting everything after it in parentheses, but if you
                                -- do that, you'll wind up with a ton of nested parentheses and your (program (will
                                -- (look (a (lot (like lisp))))).
  where
    nums =                      -- Ok, here we go.  This is NOT an assignment statement.  It's more like a macro
                                -- definition.  To really understand this, you sort of have to read it from the bottom
                                -- up.
      Set.toList $              -- Convert a set to a list.
      Set.fromList $            -- Convert a list to a set.  The point of converting a list to a set and then back again
                                -- is that we eliminate duplicates that way.
      multiplesOfLessThan 5 1000
      ++                        -- ...concatenated with...
      multiplesOfLessThan 3 1000

-- | A list of multiples of n less than m
multiplesOfLessThan n m =       -- More fun 'n' games
  takeWhile                     -- We're going to take elements from a list so long as the given function is satisfied.
  (< m)                         -- And the function is "x < m" for various values of x...
  $                             -- ...which will be coming shortly, because we're going to pause and evaluate everything
                                -- that comes after the "$"
  List.map                      -- We're going to map a function onto a list, producing another list
  (* n)                         -- ...and the function is "multipy by n"
  [1..]                         -- ...and the starting list is the infinite list of integers starting with 1.  How does
                                -- this work when we hand off an infinite list?  Yay, laziness.  Haskell only evaluates
                                -- what it needs, so the 'takeWhile' will just evaluate numbers until we hit the maximum
                                -- of m, and then it stops executing.

                                -- On the laziness above, it's worth noting that the function `multiplesOfLessThan` is
                                -- itself evaluated lazily, so it's not like it returns a complete list.  Instead, it
                                -- returns a sort of "evaluation cursor", which is itself evaluated only as needed.  So,
                                -- in theory, with the set-building above, we insert multiples of 5 into the set, and
                                -- then we insert multiples of 3 in, but when there are duplicates, we don't insert.
                                -- With luck (and maybe some profiling), we evaluate each number and then discard the
                                -- various thunks we needed, since we got a number.  It's possible that we set up an
                                -- entire evaluation tree and only collapse it when we actually print the results.  I'm
                                -- not sure at this time how to measure that, or how to fix it.
  

  -- I really should turn this into literate Haskell.
  
