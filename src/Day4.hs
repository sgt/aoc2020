{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import qualified Data.Set as S
import qualified Data.Text as T
import Protolude
import Util (isDecimal, isHex, parseInt)

validFieldsSet :: Set Text
validFieldsSet =
  S.fromList
    [ "byr",
      "iyr",
      "eyr",
      "hgt",
      "hcl",
      "ecl",
      "pid"
    ]

type RawField = Text

data Field = Field Text Text

isValidPassport :: [Field] -> Bool
isValidPassport fields = validFieldsSet `S.isSubsetOf` fieldsSet
  where
    fieldsSet = S.fromList $ (\(Field name _) -> name) <$> fields

parseField :: RawField -> Maybe Field
parseField raw = case T.splitOn ":" raw of
  [name, value] -> Just $ Field name value
  _ -> Nothing

parseInput :: Text -> [[RawField]]
parseInput = fmap words . T.splitOn "\n\n"

validEyeColors :: [Text]
validEyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

solve :: ([Field] -> Bool) -> Text -> Text
solve validatorFn input = show $ length $ filter validatorFn passports
  where
    rawPassports = parseInput input
    passports = fmap (mapMaybe parseField) rawPassports

isValidPassportA :: [Field] -> Bool
isValidPassportA = isValidPassport

solveA :: Text -> Text
solveA = solve isValidPassportA

isValidField :: Field -> Bool
isValidField (Field "byr" v) = i >= 1920 && i <= 2002
  where
    i = parseInt 0 v
isValidField (Field "iyr" v) = i >= 2010 && i <= 2020
  where
    i = parseInt 0 v
isValidField (Field "eyr" v) = i >= 2020 && i <= 2030
  where
    i = parseInt 0 v
isValidField (Field "hgt" v) = (m == "cm" && i >= 150 && i <= 193) || (m == "in" && i >= 59 && i <= 76)
  where
    m = T.takeEnd 2 v
    i = parseInt 0 (T.dropEnd 2 v)
isValidField (Field "hcl" v) = prefix == "#" && isHex postfix
  where
    (prefix, postfix) = T.splitAt 1 v
isValidField (Field "ecl" v) = v `elem` validEyeColors
isValidField (Field "pid" v) = T.length v == 9 && isDecimal v
isValidField _ = True

isValidPassportB :: [Field] -> Bool
isValidPassportB xs = isValidPassport xs && all isValidField xs

solveB :: Text -> Text
solveB = solve isValidPassportB
