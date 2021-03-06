module Lib
    ( squareSum, sumSquares
    ) where

squareSum :: Integer -> Integer
squareSum n =
  (sum [1..n])^2                -- Pretty straightforward, eh?

sumSquares :: Integer -> Integer
sumSquares n =                  -- Ok, this one's a little more complicated.  We're going to "fold" a list into a single
                                --   value (imagine folding egg whites).
  foldr (+)                     -- "r": right-associative.  In general, you should use foldr instead of foldl, if you're
                                --   not sure.  We'll be folding with the function '+'.
  0                             -- Initial value: 0.
  $                             -- Again, the dollar sign.  It's usually read as "apply", meaning we're "apply"ing the
                                --   curried function we've created so far to whatever follows.  This is all a
                                --   tremendous amount of high-fallutin' (and maybe incorrect) language, but it's like
                                --   surrounding everything that comes after the dollar sign in parentheses (and not
                                --   using the dollar sign).  BUT... while we're at it, let's talk about currying.
                                --   "Currying" is creating a new function from another function of more arguments.  So,
                                --   if you have some function f(a,b) but you only supply the first argument, then f(a)
                                --   is really a new function g(b) that's like f(a,b), but the "a" value has already
                                --   been specified, so you've turned a two-argument function into a one-argument
                                --   function.  In this case, 'foldr' takes three arguments (an operation, an initial
                                --   value, and a list), but we've only supplied two arguments at this point (the
                                --   function (+) and the initial value (0)), so we have a function of one argument (and
                                --   that argument is a list) and we're "applying" that function to what comes next (THE
                                --   LIST).  So, after all that, the "$" is usually read as "apply", but that's slightly
                                --   sloppy language, I think.
  map (^2)                      -- And, here's the list, which is generated by mapping the function "square" (^2)
                                --   onto... 
  [1..n]                        -- ...the list of natural numbers up through n.
