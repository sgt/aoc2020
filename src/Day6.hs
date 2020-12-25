{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import qualified Data.Set as S
import qualified Data.Text as T
import Protolude

type RawGroup = [Text]

allAnswers :: RawGroup -> Set Char
allAnswers = S.unions . fmap (S.fromList . toS)

parseInput :: Text -> [RawGroup]
parseInput = fmap lines . T.splitOn "\n\n"

solveA :: Text -> Text
solveA = show . sum . fmap (S.size . allAnswers) . parseInput

commonAnswers :: RawGroup -> Maybe (Set Char)
commonAnswers = foldr1May S.intersection . fmap (S.fromList . toS)

solveB :: Text -> Text
solveB = show . sum . fmap S.size . mapMaybe commonAnswers . parseInput
