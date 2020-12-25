{-# LANGUAGE OverloadedStrings #-}

module Day3Test where

import Day3
import Protolude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testData :: Text
testData =
  unlines
    [ "..##.......",
      "#...#...#..",
      ".#....#..#.",
      "..#.#...#.#",
      ".#...##..#.",
      "..#.##.....",
      ".#.#.#....#",
      ".#........#",
      "#.##...#...",
      "#...##....#",
      ".#..#...#.#"
    ]

tests :: TestTree
tests =
  testGroup
    "Day 3 tests"
    [ testCase "solve a" $ solveA testData @?= "7",
      testCase "solve a" $ solveB testData @?= "336"
    ]
