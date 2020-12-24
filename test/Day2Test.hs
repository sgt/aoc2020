{-# LANGUAGE OverloadedStrings #-}

module Day2Test where

import Day2
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testData :: Text
testData =
  unlines
    [ "1-3 a: abcde",
      "1-3 b: cdefg",
      "2-9 c: ccccccccc"
    ]

tests :: TestTree
tests =
  testGroup
    "Day 2 tests"
    [ testCase "solve a" $ solveA testData @?= "2",
      testCase "solve b" $ solveB testData @?= "1"
    ]
