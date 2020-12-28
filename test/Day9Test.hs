{-# LANGUAGE OverloadedStrings #-}

module Day9Test where

import Day9
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testData :: [Int]
testData =
  [ 35,
    20,
    15,
    25,
    47,
    40,
    62,
    55,
    65,
    95,
    102,
    117,
    150,
    182,
    127,
    219,
    299,
    277,
    309,
    576
  ]

tests :: TestTree
tests =
  testGroup
    "Day 9 tests"
    [ testCase "solve a" $ firstWeakNumber 5 testData @?= Just 127,
      testCase "solve b" $ (findContiguousSlice 127 testData >>= smallestAndLargest) @?= Just (15, 47)
    ]
