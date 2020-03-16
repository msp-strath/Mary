module Test.Mary where

import Test.Utils
import Test.Tasty (TestTree)

import Mary.ServePage

maryTests :: IO TestTree
maryTests = do
  let name      = "Mary"
  let extension = ".mary"
  let goldenExt = ".gold"
  ioTests TestConfig{..} (servePage testConfig) []
