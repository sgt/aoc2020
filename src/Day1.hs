{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import qualified Data.Set as S
import Protolude
import Util (readInts)

findComplementary :: Int -> [Int] -> Maybe (Int, Int)
findComplementary total xs = (\x -> (x, total - x)) <$> found
  where
    idx = S.fromList xs
    found = find (\x -> (total - x) `S.member` idx) xs

solveAInts :: [Int] -> Maybe Int
solveAInts intList = do
  (a, b) <- findComplementary 2020 intList
  pure $ a * b

solveA :: Text -> Text
solveA content = maybe "none found" show (solveAInts $ readInts content)

findComplementaryThree :: Int -> [Int] -> Maybe (Int, Int, Int)
findComplementaryThree total intList = go intList
  where
    go :: [Int] -> Maybe (Int, Int, Int)
    go [] = Nothing
    go (x : xs) = case findComplementary (total - x) intList of
      Nothing -> go xs
      Just (a, b) -> Just (a, b, x)

solveBInts :: [Int] -> Maybe Int
solveBInts intList = do
  (a, b, c) <- findComplementaryThree 2020 intList
  pure $ a * b * c

solveB :: Text -> Text
solveB content = maybe "none found" show (solveBInts $ readInts content)
