module Day1Test where

import Day1
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testData::[Int]
testData = [1721, 979, 366, 299, 675, 1456]

tests :: TestTree
tests =
  testGroup
    "Day 1 tests"
    [ testCase "solve a" $ solveAInts testData @?= Just 514579,
      testCase "solve b" $ solveBInts testData @?= Just 241861950
    ]
