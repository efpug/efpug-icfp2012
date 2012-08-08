module Main where

import Control.Applicative ((<$>))
import System.IO (hIsEOF, hGetLine, stdin)
import GameState

main :: IO ()
main = do
  b <- readBoard
  putStrLn $ prettyBoard b

readBoard :: IO Board
readBoard = board <$> readCellLists []
  where readCellLists sofar = do
          end <- hIsEOF stdin
          (if end
           then return sofar
           else do line <- hGetLine stdin
                   readCellLists (stringToCells line: sofar))
