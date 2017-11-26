module Main where

import Lib
import Text.Printf
import Data.Time.Clock.POSIX

n::Integer
n = 1000

main :: IO ()
main = do
  startTime <- getPOSIXTime
  printf "Product of Pythagorean triplet %s: %d"
    (show $ pythagoreanTripletSummingTo n)
    (foldr (*) 1 $ pythagoreanTripletSummingTo n)
  stopTime <- getPOSIXTime
  printf "\t(%s sec)\n" $ show $ stopTime - startTime
