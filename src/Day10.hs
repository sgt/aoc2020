{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import qualified Data.Map as M
import Protolude
import Util (readInts)

adapterChain :: [Int] -> [Int]
adapterChain initialList = case maximumMay initialList of
  Nothing -> []
  Just m -> sort $ m + 3 : initialList

countJoltDiffs :: [Int] -> Map Int Int
countJoltDiffs adapters = M.fromList diffCounts
  where
    adjAdapters = zip (0 : adapters) adapters
    diffs = foldr (\(x1, x2) acc -> x2 - x1 : acc) [] adjAdapters
    diffCounts = (fmap (\x -> (headDef 0 x, length x)) . group . sort) diffs

solveA :: Text -> Text
solveA = show . mul1And3 . countJoltDiffs . adapterChain . readInts
  where
    mul1And3 m = M.findWithDefault 0 1 m * M.findWithDefault 0 3 m

solveB :: Text -> Text
solveB _ = "not implemented"
