{-# LANGUAGE OverloadedStrings #-}
module Test.Mary where

import Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

import Test.Utils
import Test.Tasty (TestTree)

import System.Directory
import System.FilePath

import Data.Text.IO as TIO

import Mary.ServePage

testConfig :: Config
testConfig = Config
  { mary   = "mary"
  , pandoc = "pandoc"
  , user   = "testymctestyface"
  , siteRoot = "."
  }

maryRunner :: FilePath -> IO Text
maryRunner inp = do
  let page = case L.stripPrefix (siteRoot testConfig) inp of
        Just p -> T.pack p
        Nothing -> T.pack inp -- WARNING
  let inputFile = replaceExtension inp ".input"
  inputExists <- doesFileExist inputFile
  (post:get:_) <- if inputExists then
                    (map parseRequests . T.lines) <$> TIO.readFile inputFile
                 else pure [[],[]]
  servePage testConfig post (("page", page):get) inp

maryTests :: IO TestTree
maryTests = do
  let name      = "Mary"
  let extension = ".mary"
  let goldenExt = ".html"
  ioTests TestConfig{..} maryRunner
    -- excluded tests
    []
