{-# LANGUAGE OverloadedStrings #-}
module Test.Mary where

import Data.List as L
import Data.Text

import Test.Utils
import Test.Tasty (TestTree)

import Mary.ServePage

testConfig :: Config
testConfig = Config
  { mary   = "mary"
  , pandoc = "pandoc"
  , user   = "testymctestyface"
  , siteRoot = "."
  }

maryRunner :: FilePath -> IO Text
maryRunner inp = servePage testConfig [] get inp
  where
    page = case L.stripPrefix (siteRoot testConfig) inp of
      Just p -> pack p
      Nothing -> pack inp -- WARNING
    get = [("page", page)]

maryTests :: IO TestTree
maryTests = do
  let name      = "Mary"
  let extension = ".mary"
  let goldenExt = ".html"
  ioTests TestConfig{..} maryRunner
    -- excluded tests
    []
