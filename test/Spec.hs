module Main where

import qualified Day1Test
import Protolude
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "aoc2020 tests"
    [Day1Test.tests]
