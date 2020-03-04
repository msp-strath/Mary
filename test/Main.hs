module Main where

import Shonkier.Tests
import Test.Tasty

main :: IO ()
main = defaultMain =<< shonkierTests
