module Main where

import Game (gameRunner)

main :: IO ()
main = do
  (_, result) <- gameRunner
  print result
