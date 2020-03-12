module Test.Utils where

import Control.Monad

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

ioTests :: String -> [String] -> (FilePath -> IO Text) -> IO TestTree
ioTests name exts getVal = testGroup name <$> do
  files <- findByExtension exts "."
  forM files $ \ file -> do
    let base = dropExtension file
    let name = takeBaseName file
    let gold = addExtension base ".gold"
    let getGolden = do
          exists <- doesFileExist gold
          if exists then Just <$> TIO.readFile gold
          else pure Nothing
    let runTest   = getVal file
    let updGolden = TIO.writeFile gold
    pure $ goldenTest1 name getGolden runTest textDiff ShowText updGolden
