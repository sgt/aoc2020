module Main where

import qualified Day1Test
import qualified Day2Test
import qualified Day3Test
import qualified Day4Test
import qualified Day5Test
import Protolude
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "aoc2020 tests"
    [ Day1Test.tests,
      Day2Test.tests,
      Day3Test.tests,
      Day4Test.tests,
      Day5Test.tests
    ]
