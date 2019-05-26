module ComputerMode where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Internal
import           System.Random

type IntelGameState a = StateT Throws GameState a

data Throws = Throws { getThrows :: [Int]}

randomThrow :: GameState Int
randomThrow = liftIO . randomRIO $ (0, 5)

randomGuess :: GameState Int
randomGuess = liftIO . randomRIO $ (0, 10)

guessThrow :: GameState Int
guessThrow = do
  playerTurn <- gameTurn
  case playerTurn of
    P1 -> read <$> humanGuess
    P2 -> randomGuess

computerMode :: GameState ()
computerMode = do
  gameRemarks "SHORTCUTS: press q to exit, s for scores \n"

  playerTurn <- gameTurn
  gameRemarks ("It is " ++ show playerTurn ++ "'s Turn to guess")
  guess <- guessThrow
  gameRemarks $ show playerTurn ++ " has guessed."

  gameRemarks "Enter your throw"
  p1Throw <- humanThrow
  gameRemarks $ "You have entered: " ++ show p1Throw ++ "\n"
  nextScreenPrompt

  gameRemarks "P2 (Computer) is throwing"
  p2Throw <- randomThrow
  gameRemarks "P2 has thrown"
  nextScreenPrompt

  gameRemarks $ concat ["You threw: ", p1Throw, " P2 threw: ", show p2Throw]
  gameRemarks $ show playerTurn ++ " guessed: " ++ show guess
  actionWin (read p1Throw) p2Throw guess
  nextScreenPrompt

  updateTurn
  computerMode

intelligentComputerMode :: IntelGameState ()
intelligentComputerMode = do
  undefined
