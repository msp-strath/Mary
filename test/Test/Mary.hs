module Test.Mary where

import Test.Utils
import Test.Tasty (TestTree)

import Mary.ServePage

maryTests :: IO TestTree
maryTests = do
  let name      = "Mary"
  let extension = ".mary"
  let goldenExt = ".html"
  ioTests TestConfig{..} (servePage testConfig)
    -- excluded tests
    ["./examples/links.mary"] -- test pipeline does not get page metavalue
