{-# LANGUAGE OverloadedStrings #-}

module Day10Test where

import Day10
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testData1 :: Text
testData1 =
  unlines
    [ "16",
      "10",
      "15",
      "5",
      "1",
      "11",
      "7",
      "19",
      "6",
      "12",
      "4"
    ]

testData2 :: Text
testData2 =
  unlines
    [ "28",
      "33",
      "18",
      "42",
      "31",
      "14",
      "46",
      "20",
      "48",
      "47",
      "24",
      "23",
      "49",
      "45",
      "19",
      "38",
      "39",
      "11",
      "1",
      "32",
      "25",
      "35",
      "8",
      "17",
      "7",
      "9",
      "4",
      "2",
      "34",
      "10",
      "3"
    ]

tests :: TestTree
tests =
  testGroup
    "Day 10 tests"
    [ testCase "solve a" $ solveA testData1 @?= "35",
      testCase "solve a" $ solveA testData2 @?= "220",
      testCase "solve b" $ solveB testData1 @?= "8",
      testCase "solve b" $ solveB testData2 @?= "19208"
    ]
