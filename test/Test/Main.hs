module Main where

import Test.Mary
import Test.Shonkier

import Test.Tasty (testGroup)
import Test.Tasty.Silver.Interactive

main :: IO ()
main = defaultMain . testGroup "Tests" =<< sequence
  [ shonkierTests
  , maryTests
  ]
