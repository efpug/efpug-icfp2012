module Main where

import Control.Applicative ((<$>))
import System.IO (hIsEOF, hGetLine, stdin)
import System.Environment
import GameState

fullGameLoop :: Board -> IO ()
fullGameLoop board = do
  mMove <- getMoveFromStdin
  case mMove of
    Just move -> do
      newBoard <- (return (gameLoop move board))
      putStrLn $ [ moveToChar move ] 
      putStrLn $ prettyBoard newBoard
      fullGameLoop newBoard
    Nothing ->
      fullGameLoop board
  
main :: IO ()
main = do
  args <- getArgs
  b <- readBoard (head args) 
  putStrLn $ prettyBoard b
  fullGameLoop b
  
getMoveFromStdin :: IO (Maybe Move)
getMoveFromStdin = do 
  l <- hGetLine stdin
  return (charToMove (head l))

readBoard :: String -> IO Board
readBoard file = board <$> readCellLists
  where readCellLists = do
          fileStr <- readFile file
          return (map stringToCells (lines fileStr))

-- Do nothing --
gameLoop :: Move -> Board -> Board
gameLoop move board = board
