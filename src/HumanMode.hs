module HumanMode where

-- player game utils
import Internal

humanMode :: GameState ()
humanMode = do
  gameRemarks "SHORTCUTS: press q to exit, s for scores \n"

  playerTurn <- gameTurn
  gameRemarks ("It is " ++ show playerTurn ++ "'s Turn to guess")
  guess <- humanGuess
  gameRemarks $ show playerTurn ++ " has guessed."
  nextScreenPrompt

  gameRemarks "P1 Enter your throw"
  p1Throw <- humanThrow
  gameRemarks $ "You have entered: " ++ show p1Throw ++ "\n"
  nextScreenPrompt

  gameRemarks "P2 Enter your throw"
  p2Throw <- humanThrow
  gameRemarks $ "You have entered: " ++ show p2Throw ++ "\n"
  nextScreenPrompt

  gameRemarks $ concat ["P1 threw: ", p1Throw, " P2 threw: ", p2Throw]
  gameRemarks $ show playerTurn ++ " guessed: " ++ show guess
  actionWin (read p1Throw) (read p2Throw) (read guess)
  nextScreenPrompt

  updateTurn
  humanMode
