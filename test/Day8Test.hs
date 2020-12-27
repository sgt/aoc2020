{-# LANGUAGE OverloadedStrings #-}

module Day8Test where

import Day8
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testData :: Text
testData =
  unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]

tests :: TestTree
tests =
  testGroup
    "Day 8 tests"
    [ testCase "solve a" $ solveA testData @?= "InfiniteLoop 5",
      testCase "solve b" $ solveB testData @?= "not implemented"
    ]
