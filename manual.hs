module Main where

import Control.Applicative ((<$>))
import System.IO (hIsEOF, hGetLine, stdin)
import System.Environment
import GameState

main :: IO ()
main = do
  args <- getArgs
  b <- readBoard (head args) 
  putStrLn $ prettyBoard b

readBoard :: String -> IO Board
readBoard file = board <$> readCellLists
  where readCellLists = do
          fileStr <- readFile file
          return (map stringToCells (lines fileStr))
