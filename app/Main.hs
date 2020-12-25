{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import Protolude

solvers :: Map (Text, Text) (Text -> Text)
solvers =
  M.fromList
    [ (("1", "A"), Day1.solveA),
      (("1", "B"), Day1.solveB),
      (("2", "A"), Day2.solveA),
      (("2", "B"), Day2.solveB),
      (("3", "A"), Day3.solveA),
      (("3", "B"), Day3.solveB),
      (("4", "A"), Day4.solveA),
      (("4", "B"), Day4.solveB),
      (("5", "A"), Day5.solveA),
      (("5", "B"), Day5.solveB),
      (("6", "A"), Day6.solveA),
      (("6", "B"), Day6.solveB)
    ]

getFilename :: Text -> FilePath
getFilename day = toS $ "data/day" <> day <> ".txt"

parseArgs :: [Text] -> Maybe (Text, Text)
parseArgs [day, part] = Just (day, part)
parseArgs _ = Nothing

runSolver :: Text -> Text -> IO Text
runSolver day part = do
  let solverFn = M.lookup (day, T.toUpper part) solvers
  contents <- readFile (getFilename day)
  pure $ case solverFn of
    Nothing -> "Invalid arguments"
    Just fn -> fn contents

main :: IO ()
main = do
  args <- getArgs
  result <- case toS <$> args of
    [day, part] -> runSolver day part
    _ -> pure "Invalid arguments"
  putStrLn result
