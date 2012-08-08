module GameStateTests where

import Control.Applicative (liftA2)
import Data.Maybe (catMaybes)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import GameState


test :: Test
test = testGroup "Game state tests" [
  testProperty "cell-to-char-and-back" prop_cellToCharAndBack,
  testProperty "strings-to-cell-lists" prop_stringToCells,
  testProperty "cell-list-to-string-and-back" prop_cellsToStringAndBack,
  testProperty "expected-board-width" prop_boardWidth,
  testProperty "expected-board-height" prop_boardHeight,
  testProperty "expected-board-contents" prop_boardContents,
  testProperty "cell-setting changes at most one cell" prop_setCell,
  testProperty "wait does not move robot" prop_wait
  ]

prop_cellToCharAndBack :: Cell -> Bool
prop_cellToCharAndBack cell =
  maybe False (cell ==) $ charToCell $ cellToChar cell

prop_stringToCells :: Property
prop_stringToCells =
  forAll (listOf $ frequency [(90, elements legalChars),
                              (10, elements miscChars)]) $
  \s -> stringToCells s == catMaybes (map charToCell $ takeWhile (`elem` legalChars) s)
  where legalChars = "R*.# LO\\"
        miscChars = take (128 - 32) [' ' .. ]

prop_cellsToStringAndBack :: [Cell] -> Bool
prop_cellsToStringAndBack cells =
  cells == stringToCells (map cellToChar cells)

prop_boardWidth :: [[Cell]] -> Bool
prop_boardWidth cellLists =
  width (board cellLists) == if null cellLists then 0 else maximum (map length cellLists)

prop_boardHeight :: [[Cell]] -> Bool
prop_boardHeight cellLists =
  height (board cellLists) == length cellLists

prop_boardContents :: [[Cell]] -> Property
prop_boardContents cellLists =
  forAll (expandedCoords cellLists) $ \(x,y) -> cellAt b x y == cellAt' x y
  where b = board cellLists
        cellAt' x y = maybe Empty (maybe Empty id . elemAt x) $ elemAt y cellLists
        elemAt _ [] = Nothing
        elemAt 0 (x:_) = Just x
        elemAt n (_:rest) | n > 0 = elemAt (n-1) rest
                          | otherwise = Nothing

prop_setCell :: [[Cell]] -> Property
prop_setCell cellLists =
  let b = board cellLists
  in forAll (expandedCoords cellLists) $ \(x,y) ->
     forAll arbitrary $ \c ->
     let b' = setCellAt b x y c
         expectedContent (x',y') =
           (x',y') == (x,y) || cellAt b' x' y' == cellAt b x' y'
     in width b' == width b &&
        height b' == height b &&
        (x < 0 || x >= width b || y < 0 || y >= height b || cellAt b' x y == c) &&
        all expectedContent [(x',y') | x' <- [0..width b], y' <- [0..height b]]

prop_wait :: [[Cell]] -> Bool
prop_wait cellLists =
    let b = board cellLists
    in move b W == (maybe b (const $ b { score = score b - 1 }) (robotXY b))

expandedCoords :: [[Cell]] -> Gen (Int,Int)
expandedCoords cellLists = liftA2 (,) cols rows
  where cols = choose $ expandedRange width
        rows = choose $ expandedRange height
        width = if null cellLists then 0 else maximum (map length cellLists)
        height = length cellLists
        expandedRange x = (negate (x `div` 2), x + (x `div` 2))

instance Arbitrary Cell where
  arbitrary = oneof $ map return [Rock,
                                  Robot,
                                  Earth,
                                  Empty,
                                  Wall,
                                  Lift Open,
                                  Lift Closed,
                                  Lambda]
