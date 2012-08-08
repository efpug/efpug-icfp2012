module Main where

import Control.Applicative ((<$>))
import System.IO (hIsEOF, hGetLine, stdin)
import System.Environment
import GameState

fullGameLoop :: Board -> IO ()
fullGameLoop board = do
  move <- getMoveFromStdin
  newBoard <- (return (gameLoop move board))
  putStrLn $ prettyBoard newBoard
  fullGameLoop newBoard
  
main :: IO ()
main = do
  args <- getArgs
  b <- readBoard (head args) 
  putStrLn $ prettyBoard b
  fullGameLoop b
  
getMoveFromStdin :: IO Move
getMoveFromStdin = do 
  return W

readBoard :: String -> IO Board
readBoard file = board <$> readCellLists
  where readCellLists = do
          fileStr <- readFile file
          return (map stringToCells (lines fileStr))

-- Do nothing --
gameLoop :: Move -> Board -> Board
gameLoop move board = board
