module Lib where

import System.Random
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import System.Exit

newtype Identity a = Identity a deriving (Eq, Show)

randomThrow :: GameState Int
randomThrow = liftIO . randomRIO $ (0, 5)

randomGuess :: GameState Int
randomGuess = liftIO . randomRIO $ (0, 10)

type HumanScore = Int
type ComScore = Int
data Scores = Scores HumanScore ComScore deriving (Eq, Show)

displayScore :: Scores -> String
displayScore (Scores h c) = concat ["current score is: \n"
                                   , "Human: ", show h, "\n"
                                   , "Computer: ", show c]

data GameLocal = GameLocal { scores :: Scores, turn :: Turn } deriving (Eq, Show)
data Turn = Human | Computer deriving (Eq, Show)

type GameState a = StateT GameLocal IO a

addPScore :: Scores -> Scores
addPScore (Scores p c) = Scores (p + 1) c

addCScore :: Scores -> Scores
addCScore (Scores p c) = Scores p (c + 1)

changeTurn :: Turn -> Turn
changeTurn t = case t of
  Human -> Computer
  Computer -> Human

updateWin :: GameState ()
updateWin = do
  playerTurn <- gameTurn
  case playerTurn of
    Human -> StateT $
      \gstate -> return ((), GameLocal (addPScore $ scores gstate) (turn gstate))
    Computer -> StateT $
      \gstate -> return ((), GameLocal (addCScore $ scores gstate) (turn gstate))

updateTurn :: GameState ()
updateTurn = StateT $
    \gstate -> return ((), GameLocal (scores gstate) (changeTurn $ turn gstate))

snapState :: GameState GameLocal
snapState = StateT $ \s -> return (s, s)

gameResults :: GameState Scores
gameResults = fmap scores snapState

gameTurn :: GameState Turn
gameTurn = fmap turn snapState

checkScore :: GameState ()
checkScore = do
  score <- gameResults
  liftIO $ putStrLn (displayScore score)

guessThrow :: Turn -> GameState Int
guessThrow t = case t of
  Human -> read <$> userInput
  Computer -> randomGuess

userInput :: GameState String
userInput =
  liftIO getLine >>=
  \input -> let result | input == "q" = do
                           checkScore
                           liftIO $ print "Exited"
                           liftIO exitSuccess
                       | input == "s" = checkScore >> userInput
                       | input `elem` ((: []) <$> ['1'..'5']) = return input
                       | otherwise = do
                           liftIO $ print "Invalid input!"
                           userInput
            in result

actionWin :: Int -> Int -> Int -> GameState ()
actionWin playerT comT guess
  | playerT + comT == guess = do
      playerTurn <- gameTurn
      liftIO . print $ "Successful guess by: " ++ show playerTurn
      updateWin
  | otherwise = do
      playerTurn <- gameTurn
      liftIO . print $ "Failed guess by: " ++ show playerTurn


game :: GameState ()
game = do
  liftIO $ putStrLn "press q to exit, c to continue , s for scores"
  playerTurn <- gameTurn
  liftIO . print $ ("It is " ++ show playerTurn ++ "'s Turn to guess")
  guess <- guessThrow playerTurn
  liftIO . print $ show playerTurn ++ " has guessed."
  liftIO . print $ "Enter your throw"
  p1 <- userInput
  c1 <- do
    liftIO . print $ "Computer has thrown"
    randomThrow
  liftIO . print $ concat ["You threw: ", p1, " Computer threw: ", show c1]
  liftIO . print $ show playerTurn ++ " guessed: " ++ show guess
  actionWin (read p1) c1 guess
  updateTurn
  game

initGame :: GameLocal
initGame = GameLocal (Scores 0 0) Computer

gameRunner :: IO ((), GameLocal)
gameRunner = runStateT game initGame

