module Test.Mary where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO
import System.Process

import Test.Utils
import Test.Tasty (TestTree)

mary :: FilePath -> IO Text
mary inp = do
  (_, Just hpandoc, _, _) <-
    createProcess (proc "pandoc" ["-s", inp, "-f", "markdown", "-t", "json"])
                  { std_out = CreatePipe
                  }
  (_, Just hmary, _, _) <-
    createProcess (proc "mary" ["-pandoc"])
                  { std_in  = UseHandle hpandoc
                  , std_out = CreatePipe
                  }
  (_ , Just hout, _, _) <-
    createProcess (proc "pandoc" ["-s", "-f", "json", "-t", "html"])
                  { std_in = UseHandle hmary
                  , std_out = CreatePipe
                  }
  txt <- TIO.hGetContents hout
  mapM_ hClose [hpandoc, hmary, hout]
  pure txt


maryTests :: IO TestTree
maryTests = ioTests "Mary" [".mary"] mary
