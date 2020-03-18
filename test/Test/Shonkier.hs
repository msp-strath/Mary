module Test.Shonkier where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Process

import Test.Tasty (TestTree)
import Test.Utils

shonkier :: FilePath -> IO Text
shonkier inp = T.pack <$> readProcess "mary" ["-shonkier", inp] ""

shonkierTests :: IO TestTree
shonkierTests = do
  let name = "Shonkier"
  let extension = ".shonkier"
  let goldenExt = ".gold"
  ioTests TestConfig{..} shonkier
    -- excluded tests:
    []

shonkierJS :: FilePath -> IO Text
shonkierJS inp =
  withCreateProcess ((proc "mary" ["-shonkierjs", inp])
                     { std_out = CreatePipe
                     }) $ \ _ (Just hmary) _ _ ->
  withCreateProcess ((proc "node" ["-"])
                     { std_in = UseHandle hmary
                     , std_out = CreatePipe
                     }) $ \ _ (Just hout) _ _ ->
  TIO.hGetContents hout

shonkierJSTests :: IO TestTree
shonkierJSTests = do
  let name = "ShonkierJS"
  let extension = ".shonkier"
  let goldenExt = ".jsgold"
  ioTests TestConfig{..} shonkierJS $
    -- excluded tests:
    fmap (\ t -> "./examples/" ++ t ++ ".shonkier")
      []
