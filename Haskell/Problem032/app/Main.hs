module Main where

import Text.Printf
import Data.Time.Clock.POSIX
import Lib

main :: IO ()
main = do

  printf "head panDigitalTriples = %s\n" $ show $ head panDigitalTriples
  
  startTime <- getPOSIXTime
  
  printf "sum of products of all pandigital triples x * y = z: %d" sumPandigitalProducts

  stopTime <- getPOSIXTime
  printf "\t(%s sec)\n" $ show (stopTime - startTime)
  
