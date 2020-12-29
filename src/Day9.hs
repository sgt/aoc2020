{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import Day1 (findComplementary)
import Protolude
import Util (readInts)

xmasList :: Int -> [Int] -> [([Int], Int)]
xmasList preludeLength xs
  | length xs < preludeLength + 1 = []
  | otherwise = (prelude, i) : xmasList preludeLength remainder
  where
    (prelude, xs') = splitAt preludeLength xs
    (i, remainder) = (headDef (-1) xs', tailSafe xs)

firstWeakNumber :: Int -> [Int] -> Maybe Int
firstWeakNumber preludeLength xs = snd <$> find isWeak (xmasList preludeLength xs)
  where
    isWeak (prelude, x) = isNothing $ findComplementary x prelude

findContiguousSlice :: Int -> [Int] -> Maybe [Int]
findContiguousSlice target = f []
  where
    f _ [] = Nothing
    f acc numList@(x : xs)
      | sum acc == target = Just acc
      | sum acc > target = f (drop 1 acc) numList
      | otherwise = f (acc ++ [x]) xs

smallestAndLargest :: Ord a => [a] -> Maybe (a, a)
smallestAndLargest = liftA2 (,) <$> minimumMay <*> maximumMay

solutionA :: [Int] -> Maybe Int
solutionA = firstWeakNumber 25

solveA :: Text -> Text
solveA = show . solutionA . readInts

solveB :: Text -> Text
solveB input = show $ do
  target <- solutionA (readInts input)
  slice <- findContiguousSlice target $ readInts input
  (a1, a2) <- smallestAndLargest slice
  pure $ a1 + a2
