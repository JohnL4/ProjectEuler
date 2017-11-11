module Main where

import Lib
import Data.List

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
      map                       -- We're going to map a function onto a list, producing a list.
      (!!0) $                   -- ...and the function is "take the first element of the list we give you" ("xs!!0" is
                                -- like "xs[0]" in languages like C.
      group $                   -- A two-level list of numbers, where equal numbers are grouped into their own
                                -- sublists.  So we're basically making a list by taking the first element of each of
                                -- the inner lists.  In order for this to work to return unique numbers, we have to feed
                                -- it a sorted list.  (NOTE: In future, might be better to do this with a Set instead,
                                -- so we don't have to build complete lists.  Makes a difference when the lists are
                                -- huge.)
      sort $                    -- Sort the following list, which is composed of...
      multiplesOfLessThan 5 1000
      ++                        -- ...concatenated with...
      multiplesOfLessThan 3 1000

-- | A list of multiples of n less than m
multiplesOfLessThan n m =       -- More fun 'n' games
  takeWhile                     -- We're going to take elements from a list so long as the given function is satisfied.
  (< m)                         -- And the function is "x < m" for various values of x...
  $                             -- ...which will be coming shortly, because we're going to pause and evaluate everything
                                -- that comes after the "$"
  map                           -- We're going to map a function onto a list, producing another list
  (* n)                         -- ...and the function is "multipy by n"
  [1..]                         -- ...and the starting list is the infinite list of integers starting with 1.  How does
                                -- this work when we hand off an infinite list?  Yay, laziness.  Haskell only evaluates
                                -- what it needs, so the 'takeWhile' will just evaluate numbers until we hit the maximum
                                -- of m, and then it stops executing.

  -- I really should turn this into literate Haskell.
  
