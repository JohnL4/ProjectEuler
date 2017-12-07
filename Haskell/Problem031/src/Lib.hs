module Lib
    ( changeCombinations
    ) where

-- | A pile of coins making up the amount we want.  This isn't declaring data but a data TYPE.
data PileOfCoins = PileOfCoins { pounds2 :: Integer -- ^ 2-pound coins   
                               , pound :: Integer   -- ^ 1-pound coins   
                               , pence50 :: Integer -- ^ 50-pence coins  
                               , pence20 :: Integer -- ^ 20-p coins      
                               , pence10 :: Integer -- ^ 10-p coins      
                               , pence5 :: Integer  -- ^ 5-p coins       
                               , pence2 :: Integer  -- ^ 2-p coins
                                        -- We leave off 1-p coins because we assume whatever shortfall we have in this
                                        -- pile o' coins can be made up with the smallest denomination.  If the smallest
                                        -- denomination wasn't 1, we'd have a problem.
                               } deriving (Show) -- This clause makes Haskell figure out a way to display this thing
                                                 -- (useful for debugging).

-- | A list of all different piles of coins that make up the given 'amount'.  Again, because of Haskell's laziness,
-- making a list of piles of coins isn't a waste becasue each element is only built when it's needed, and dropped as
-- soon as it's not needed (which, if you're just using 'length' to count the list size, basically means each pile is
-- built and discarded immediately).  Each "pile" is really just a snapshot of a bunch of index variables, so (depending
-- on how we call this in the Main module), we're basically just building an accumulated total w/out actually
-- accumulating much data.  Unless, of course, we want to convert the whole thing to a string, for display/debugging.
--
-- A note on implementation: this is kind of a hack because it's hardcoding the list of denominations and assuming the
-- smallest one is 1.  I could probably have made this more general purpose, but I was too lazy to really be elegant.
changeCombinations amount =
  [ PileOfCoins {              -- A list of all piles of coins... (Square bracket means "list")
        pounds2 = pounds2 
        , pound = pound
        , pence50 = pence50
        , pence20 = pence20
        , pence10 = pence10
        , pence5 = pence5
        , pence2 = pence2
        }
    |                           -- ...such that...

                                -- (Here we begin basically counting "numbers", each of whose digits comes from a
                                --   different "base".  So the first digit is "base 200", the second digit is "base
                                --   100", etc.  I know that's pretty sloppy language.)

    pounds2 <- [0..(amount `quot` 200)] -- ...2-pound coins come from a number in this range (`quot` is integer
                                        --   division)
    , pound <- [0..((amount - 200 * pounds2) `quot` 100)] -- ...and 1-pound coins come from this range.  Note that this
                                                          --   is dependent on the number of 2-pound coins we have, so
                                                          --   fewer 2-pound coins means we can have more 1-pound coins.
    , pence50 <- [0..((amount - 200 * pounds2 - 100 * pound) `quot` 50)] -- ...etc.
    , pence20 <- [0..((amount - 200 * pounds2 - 100 * pound - 50 * pence50) `quot` 20)]
    , pence10 <- [0..((amount - 200 * pounds2 - 100 * pound - 50 * pence50 - 20 * pence20) `quot` 10)]
    , pence5 <- [0..((amount - 200 * pounds2 - 100 * pound - 50 * pence50 - 20 * pence20 - 10 * pence10) `quot` 5)]
    , pence2 <- [0..((amount - 200 * pounds2 - 100 * pound - 50 * pence50 - 20 * pence20 - 10 * pence10 - 5 * pence5) `quot` 2)]
    ]
  
  
