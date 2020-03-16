-- | Hello!

module Mary.ServePage where

import Data.Text
import Data.Text.IO as TIO

import System.Process

data Config = Config
  { mary   :: FilePath
  , pandoc :: FilePath
  }

testConfig :: Config
testConfig = Config
  { mary   = "mary"
  , pandoc = "pandoc"
  }

servePage :: Config -> FilePath -> IO Text
servePage Config{..} inp =
  withCreateProcess ((proc pandoc ["-s", inp, "-f", "markdown", "-t", "json"])
                     { std_out = CreatePipe
                     }) $ \ _ (Just hpandoc) _ _ ->
  withCreateProcess ((proc mary ["pandoc"])
                     { std_in  = UseHandle hpandoc
                     , std_out = CreatePipe
                     }) $ \ _ (Just hmary) _ _ ->
  withCreateProcess ((proc pandoc ["-s", "-f", "json", "-t", "html", "--template", "templates/mary.html5"])
                     { std_in = UseHandle hmary
                     , std_out = CreatePipe
                     }) $ \ _ (Just hout) _ _ ->
  TIO.hGetContents hout
