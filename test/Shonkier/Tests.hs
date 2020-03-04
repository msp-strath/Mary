module Shonkier.Tests where

import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath
import System.Process

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Silver
import Test.Tasty.Silver.Advanced

shonkier :: FilePath -> IO Text
shonkier inp = T.pack <$> readProcess "mary" ["-shonkier", inp] ""

textDiff :: Text -> Text -> GDiff
textDiff s t
  | s == t    = Equal
  | otherwise = DiffText Nothing s t

shonkierTests :: IO TestTree
shonkierTests = testGroup "Shonkier" <$> do
  files <- findByExtension [".shonkier"] "."
  forM files $ \ file -> do
    let base = dropExtension file
    let name = takeBaseName file
    let gold = addExtension base ".gold"
    let getGolden = do
          exists <- doesFileExist gold
          if exists then Just <$> TIO.readFile gold
          else pure Nothing
    let runTest   = shonkier file
    let updGolden = TIO.writeFile gold
    pure $ goldenTest1 name getGolden runTest textDiff ShowText updGolden
