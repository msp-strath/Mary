module Main where

import Shonkier.Tests
import Test.Tasty.Silver.Interactive

main :: IO ()
main = defaultMain =<< shonkierTests
