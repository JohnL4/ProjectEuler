module Main where

import Text.Printf
import Data.Time.Clock.POSIX
import Lib

main :: IO ()
main = do
  
  startTime <- getPOSIXTime

  printf "sum of products in pandigital eualities: %d" sumPandigitalProducts

  stopTime <- getPOSIXTime
  printf "\t(%s sec)\n" show $ stopTime - startTime
  
