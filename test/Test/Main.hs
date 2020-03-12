module Main where

import Test.Mary
import Test.Shonkier
import Test.Tasty

main :: IO ()
main = defaultMain . testGroup "Tests" =<< sequence
  [ shonkierTests
  , maryTests
  ]
