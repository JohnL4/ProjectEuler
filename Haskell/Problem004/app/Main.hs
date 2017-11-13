module Main where

import Lib
import Text.Printf

main :: IO ()
main = do
  printf "hi\n"

isPalindrome x =
  leftHalf == reverse rightHalf
  where
    n = length (show x)
    halfLength = floor (n / 2)  -- TODO: use 'quot' operation, not (/)
    leftHalf = take halfLength (show x)
    rightHalf = drop (halfLength + (if (n `mod` 2) == 1 then 1 else 0)) (show x)
                     
    
