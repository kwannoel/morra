module Internal where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           System.Console.ANSI       (clearScreen)
import           System.Exit

newtype Identity a = Identity a deriving (Eq, Show)

data GameLocal = GameLocal { scores :: Scores, turn :: Turn } deriving (Eq, Show)

type P1Score = Int
type P2Score = Int
data Scores = Scores P1Score P2Score deriving (Eq, Show)

data Turn = P1 | P2 deriving (Eq, Show)

type GameState = StateT GameLocal IO

-- Remarks & Comments helpers

gameRemarks :: String -> GameState ()
gameRemarks = liftIO . putStrLn

displayScore :: Scores -> String
displayScore (Scores p1 p2) = concat ["current score is: \n"
                                   , "P1: ", show p1, "\n"
                                   , "P2: ", show p2]

-- Score utilities

addPScore :: Scores -> Scores
addPScore (Scores p c) = Scores (p + 1) c

addCScore :: Scores -> Scores
addCScore (Scores p c) = Scores p (c + 1)

changeTurn :: Turn -> Turn
changeTurn t =
  case t of
    P1 -> P2
    P2 -> P1

-- Updating Game state

updateWin :: GameState ()
updateWin = do
  playerTurn <- gameTurn
  case playerTurn of
    P1 -> StateT $
      \gstate -> return ((), GameLocal (addPScore $ scores gstate) (turn gstate))
    P2 -> StateT $
      \gstate -> return ((), GameLocal (addCScore $ scores gstate) (turn gstate))

updateTurn :: GameState ()
updateTurn = StateT $
    \gstate -> return ((), GameLocal (scores gstate) (changeTurn $ turn gstate))

-- Examining Properties of game State

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

-- Safe handling of user inputs in game context

userInput :: (String -> Bool) -> GameState String
userInput criteria =
  liftIO getLine >>=
  \input -> let result | input == "q" = do
                           checkScore
                           liftIO $ print "Exited"
                           liftIO exitSuccess
                       | input == "s" = checkScore >> userInput criteria
                       | criteria input  = return input
                       | otherwise = do
                           gameRemarks "Invalid input! \nTry Again:"
                           userInput criteria
            in result

-- 1 - 5 for throws, 1 - 10 for guesses

throwsLs :: [String]
throwsLs = (: []) <$> ['1'..'5']

guessLs :: [String]
guessLs = ((: []) <$> ['1'..'9']) ++ ["10"]


humanThrow :: GameState String
humanThrow = userInput (`elem` throwsLs)

humanGuess :: GameState String
humanGuess = userInput (`elem` guessLs)

-- Handling guess outcomes

actionWin :: Int -> Int -> Int -> GameState ()
actionWin playerT comT guess =
  let actions t | playerT + comT == guess = do
                    gameRemarks $ "Successful guess by: " ++ show t
                    updateWin
                | otherwise = gameRemarks $ "Failed guess by: " ++ show t
  in do
    playerTurn <- gameTurn
    actions playerTurn

-- Screen refresh utils
nextScreenPrompt :: GameState ()
nextScreenPrompt = do
  gameRemarks "Press <Enter> to Continue"
  c <- liftIO getChar
  case c of
      '\n' -> liftIO clearScreen
      _    -> nextScreenPrompt
