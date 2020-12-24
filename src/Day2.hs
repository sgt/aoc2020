{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import Protolude

data Constraints = Constraints Char Int Int

isValidA :: Text -> Constraints -> Bool
isValidA password (Constraints ch minE maxE) = entries >= minE && entries <= maxE
  where
    entries = length $ filter (== ch) (toS password)

splitTwoOn :: Text -> Text -> Maybe (Text, Text)
splitTwoOn spl x = case T.splitOn spl x of
  [a, b] -> Just (a, b)
  _ -> Nothing

parseLine :: Text -> Maybe (Constraints, Text)
parseLine x = do
  (constraintDef, password) <- splitTwoOn ": " x
  (minMax, chT) <- splitTwoOn " " constraintDef
  (minET, maxET) <- splitTwoOn "-" minMax
  ch <- if T.length chT == 1 then Just (T.head chT) else Nothing
  minE <- readMaybe $ toS minET
  maxE <- readMaybe $ toS maxET
  pure (Constraints ch minE maxE, password)

solve :: (Text -> Constraints -> Bool) -> Text -> Text
solve validatorFn input = show $ length $ filter (== True) $ fmap (\(cs, t) -> validatorFn t cs) params
  where
    params = catMaybes $ parseLine <$> lines input

solveA :: Text -> Text
solveA = solve isValidA

isValidB :: Text -> Constraints -> Bool
isValidB password (Constraints ch minE maxE) =
  T.length password >= maxE && ((ch == ch1) `xor` (ch == ch2))
  where
    ch1 = T.index password (minE - 1)
    ch2 = T.index password (maxE - 1)

solveB :: Text -> Text
solveB = solve isValidB
