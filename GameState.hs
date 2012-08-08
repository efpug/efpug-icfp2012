module GameState (Cell(..),
                  LiftState(..),
                  charToCell,
                  moveToChar,
                  cellToChar,
                  charToMove,
                  stringToCells,
                  Board(score,width,height),
                  robotXY,
                  board,
                  cellAt,
                  setCellAt,
                  prettyBoard,
                  Move(..),
                  move) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as DIM
import Data.List (find, elemIndex, intercalate)
import Data.Maybe (isJust, catMaybes)

data Cell =
  Robot |
  Rock |
  Earth |
  Wall |
  Lambda |
  Lift LiftState |
  Empty deriving (Eq, Ord, Show)

data LiftState = Open | Closed deriving (Eq, Ord, Show)

charToCell :: Char -> Maybe Cell
charToCell c | c == '#' = Just Wall
             | c == '.' = Just Earth
             | c == '*' = Just Rock
             | c == ' ' = Just Empty
             | c == '\\' = Just Lambda
             | c == 'L' = Just $ Lift Closed
             | c == 'O' = Just $ Lift Open
             | c == 'R' = Just Robot
             | otherwise = Nothing

charToMove :: Char -> Maybe Move
charToMove m | m == 'A' = Just A
             | m == 'L' = Just L
             | m == 'R' = Just R
             | m == 'D' = Just D
             | m == 'U' = Just U
             | m == 'W' = Just W
             | otherwise = Nothing

cellToChar :: Cell -> Char
cellToChar cell = case cell of
  Wall        -> '#'
  Earth       -> '.'
  Rock        -> '*'
  Empty       -> ' '
  Lambda      -> '\\'
  Lift Closed -> 'L'
  Lift Open   -> 'O'
  Robot       -> 'R'


moveToChar :: Move -> Char
moveToChar move = case move of
  W        -> 'W'
  U        -> 'U'
  D        -> 'D'
  R        -> 'R'
  A        -> 'A'
  L        -> 'L'




-- | Converts a string to a list of cells, stopping at the first
-- invalid character.
stringToCells :: String -> [Cell]
stringToCells = catMaybes . takeWhile isJust . map charToCell

data Board =
  Board { robotPos :: !RobotPos,
          width  :: !Int,
          height :: !Int,
          score :: !Int,
          lambdasCollected :: !Int,
          lambdasRemaining :: !Int,
          rows :: IntMap (IntMap Cell) } deriving (Eq, Show)

data RobotPos = NoRobot | RobotAt !Int !Int deriving (Eq, Show)

robotXY :: Board -> Maybe (Int,Int)
robotXY b = case robotPos b of
  NoRobot -> Nothing
  RobotAt x y -> Just (x, y)

toRobotPos :: Maybe (Int, Int) -> RobotPos
toRobotPos = maybe NoRobot (\(x,y) -> RobotAt x y)

board :: [[Cell]] -> Board
board cellLists =
  Board { robotPos = theRobotPos,
          width  = if null cellLists then 0 else maximum (map length cellLists),
          height = length cellLists,
          score = 0,
          lambdasCollected = 0,
          lambdasRemaining = sum $ map (length . filter (==Lambda)) cellLists,
          rows = mkIntMap (map mkIntMap cellLists) }
  where mkIntMap = DIM.fromList . zip [1..]
        theRobotPos = toRobotPos $ do
          (y, Just x) <- find (isJust . snd) $
                         zip [0..] $ map (elemIndex Robot) cellLists
          return (x + 1, y + 1)

cellAt :: Board -> Int -> Int -> Cell
cellAt b x y = maybe Empty (maybe Empty id . DIM.lookup x) (DIM.lookup y $ rows b)

setCellAt :: Board -> Int -> Int -> Cell -> Board
setCellAt b x y c
  | 0 < x && x <= width b && 0 < y && y <= height b =
    b { rows = DIM.alter alterRow y $ rows b }
  | otherwise = b
  where alterRow Nothing = Just $ DIM.singleton x c
        alterRow (Just row) = Just $ DIM.alter alterCell x row
        alterCell = const $ Just c

prettyBoard :: Board -> String
prettyBoard =
  intercalate "\n" .
  reverse .
  map (map (cellToChar . snd) . DIM.toList . snd) .
  DIM.toList .
  rows

data Move = L |
            R |
            U |
            D |
            W |
            A deriving (Eq, Show)

moveDelta :: Move -> (Int,Int)
moveDelta L = (negate 1, 0)
moveDelta R = (1, 0)
moveDelta U = (0, 1)
moveDelta D = (0, negate 1)
moveDelta W = (0, 0)
moveDelta A = (0, 0)

move :: Board -> Move -> Board
move b m = maybe b go (robotXY b)
    where
      go (rx,ry) =
          b'''' { score = score b'''' + dScore - 1,
                  lambdasRemaining = lambdasRemaining b'''' - dLambdas,
                  lambdasCollected = lambdasCollected b'''' + dLambdas }
          where
            (dx, dy) = moveDelta m
            unobstructed = elem c [Empty, Earth, Lambda, Lift Open]
            movesRock = dy == 0 && c == Rock && cellAt b rx'' ry' == Empty
            isMove = (dx /= 0 || dy /= 0) && (unobstructed || movesRock)

            b'    = setCellAt b rx ry Empty
            b''   = setCellAt b' rx' ry' Robot
            b'''  = if movesRock then setCellAt b'' rx'' ry' Rock else b''
            b'''' = if isMove then b''' else b

            c = cellAt b rx' ry'
            rx'  = rx + dx
            ry'  = ry + dy
            rx'' = rx' + dx
            (dLambdas, dScore) = case c of
              Lambda -> (1, 25)
              Lift Open -> (0, 50 * lambdasCollected b)
              _ -> if m == A then (0, 25 * lambdasCollected b) else (0,0)

