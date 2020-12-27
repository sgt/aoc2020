{-# LANGUAGE OverloadedStrings #-}

module Util where

import Data.Char (isDigit, isHexDigit)
import qualified Data.Text as T
import Protolude

readInts :: Text -> [Int]
readInts content = mapMaybe (readMaybe . toS) (lines content)

parseInt :: Int -> Text -> Int
parseInt def x = fromMaybe def $ (readMaybe . toS) x

parseSignedInt :: Text -> Maybe Int
parseSignedInt x = case sign of
  "+" -> i
  "-" -> negate <$> i
  _ -> Nothing
  where
    (sign, iT) = T.splitAt 1 x
    i = readMaybe $ toS iT

isHex :: Text -> Bool
isHex = T.all isHexDigit

isDecimal :: Text -> Bool
isDecimal = T.all isDigit

splitTwoOn :: Text -> Text -> Maybe (Text, Text)
splitTwoOn spl x = case T.splitOn spl x of
  [a, b] -> Just (a, b)
  _ -> Nothing
