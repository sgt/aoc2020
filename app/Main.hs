{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import Day1
import Protolude

solvers :: Map (Text, Text) (Text -> Text)
solvers =
  M.fromList
    [ (("1", "A"), Day1.solveA),
      (("1", "B"), Day1.solveB)
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
