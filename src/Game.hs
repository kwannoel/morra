module Game where

import Internal (GameLocal (..), Scores (..), Turn (..))
import ComputerMode (computerMode)
import HumanMode (humanMode)
import Control.Monad.Trans.State (runStateT)

initGame :: GameLocal
initGame = GameLocal (Scores 0 0) P2

gameRunner :: IO ((), GameLocal)
gameRunner = do
  putStrLn "Enter p to play hotseat\nEnter c to versus Computer"
  mode <- getLine
  case mode of
    "c" -> runStateT computerMode initGame
    "p" -> runStateT humanMode initGame
    _   -> do
      putStrLn "Invalid choice"
      gameRunner
