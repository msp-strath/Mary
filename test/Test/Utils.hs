module Test.Utils where

import Control.Monad

import Data.List ((\\))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Silver
import Test.Tasty.Silver.Advanced

textDiff :: Text -> Text -> GDiff
textDiff s t
  | s == t    = Equal
  | otherwise = DiffText Nothing s t

data TestConfig = TestConfig
  { name      :: String
  , extension :: String
  , goldenExt :: String
  }

ioTests :: TestConfig -> (FilePath -> IO Text) -> [FilePath] -> IO TestTree
ioTests TestConfig{..} getVal excluded = testGroup name <$> do
  files <- findByExtension [extension] "."
  forM (files \\ excluded) $ \ file -> do
    let base = dropExtension file
    let name = takeBaseName file
    let gold = addExtension base goldenExt
    let getGolden = do
          exists <- doesFileExist gold
          if exists then Just <$> TIO.readFile gold
          else pure Nothing
    let runTest   = getVal file
    let updGolden = TIO.writeFile gold
    pure $ goldenTest1 name getGolden runTest textDiff ShowText updGolden
