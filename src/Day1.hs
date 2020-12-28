{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import qualified Data.Map as M
import Protolude
import Util (readInts)

mkCountMap :: [Int] -> Map Int Int
mkCountMap = M.fromList . mapMaybe f . group . sort
  where
    f [] = Nothing
    f xs@(x : _) = Just (x, length xs)

findComplementary :: Int -> [Int] -> Maybe (Int, Int)
findComplementary total xs = (\x -> (x, total - x)) <$> find isComplementary xs
  where
    idx = mkCountMap xs
    isComplementary x
      | isNothing cnt = False
      | cx == x && fromMaybe 0 cnt < 2 = False
      | otherwise = True
      where
        cx = total - x
        cnt = cx `M.lookup` idx

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
