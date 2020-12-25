{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import qualified Data.Text as T
import Protolude

-- move and coords are (right, down)

data Move = Move Int Int

-- coords are zero-indexed
data Coords = Coords Int Int

type Grid = [Text]

moveA :: Move
moveA = Move 3 1

gridWidth :: Grid -> Maybe Int
gridWidth grid = T.length <$> headMay grid

hasTree :: Grid -> Coords -> Bool
hasTree grid (Coords x y) = not outOfGrid && gw > 0 && T.index row realX == '#'
  where
    gw = fromMaybe 0 (gridWidth grid)
    realX = x `mod` gw
    outOfGrid = y > (length grid - 1)
    row = atDef (toS $ replicate gw '.') grid y

coordsList :: Grid -> Move -> [Coords]
coordsList grid (Move right down) = takeWhile (\(Coords _ y) -> y < length grid) infCoords
  where
    infCoords = iterate addCoords initCoords
    initCoords = Coords right down
    addCoords (Coords x y) = Coords (x + right) (y + down)

solveSingle :: Grid -> Move -> Int
solveSingle grid move = length $ filter (hasTree grid) coords
  where
    coords = coordsList grid move

solveA :: Text -> Text
solveA input = show $ solveSingle (lines input) moveA

movesB :: [Move]
movesB =
  [ Move 1 1,
    Move 3 1,
    Move 5 1,
    Move 7 1,
    Move 1 2
  ]

solveB :: Text -> Text
solveB input = show $ product $ solveSingle (lines input) <$> movesB
