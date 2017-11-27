module Main where

import Lib
import Text.Printf
import Data.Time.Clock.POSIX

n = 4::Int

main :: IO ()
main = do
  startTime <- getPOSIXTime
  
  printf "Maximum product of %d values taken in a straight line from array 'values':\n\t%d"
    n $ maxStraightProduct n

  stopTime <- getPOSIXTime
  printf "\t(%s sec)\n" $ show (stopTime - startTime)
