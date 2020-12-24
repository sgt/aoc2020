module Day1Test where

import Day1
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Day 1 tests"
    [ testCase "solve a" $ solveAInts [1721, 979, 366, 299, 675, 1456] @?= Just 514579,
      testCase "solve b" $ solveBInts [1721, 979, 366, 299, 675, 1456] @?= Just 241861950
    ]
