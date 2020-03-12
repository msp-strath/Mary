module Test.Mary where

import Test.Utils
import Test.Tasty (TestTree)

import Mary.ServePage

maryTests :: IO TestTree
maryTests = ioTests "Mary" [".mary"] servePage
