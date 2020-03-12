module Test.Shonkier where

import Data.Text (Text)
import qualified Data.Text as T

import System.Process

import Test.Tasty (TestTree)
import Test.Utils

shonkier :: FilePath -> IO Text
shonkier inp = T.pack <$> readProcess "mary" ["-shonkier", inp] ""

shonkierTests :: IO TestTree
shonkierTests = ioTests "Shonkier" [".shonkier"] shonkier
