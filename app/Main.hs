module Main where

import Lib (gameRunner)

main :: IO ()
main = do
  result <- gameRunner
  print result
