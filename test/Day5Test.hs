{-# LANGUAGE OverloadedStrings #-}

module Day5Test where

import Day5
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Day 5 tests"
    [ testCase "seat 1" $ decodeSeatId "BFFFBBFRRR" @?= 567,
      testCase "seat 2" $ decodeSeatId "FFFBBBFRRR" @?= 119,
      testCase "seat 3" $ decodeSeatId "BBFFBBFRLL" @?= 820
    ]
  where
    decodeSeatId = seatId . decodeSeat
