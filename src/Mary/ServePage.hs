-- | Hello!

module Mary.ServePage where

import Control.Arrow

import Data.ByteString.Lazy as B
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO as TIO

import Data.PHPSession

import System.Process
import System.IO

data Config = Config
  { mary   :: FilePath
  , pandoc :: FilePath
  , user   :: String
  , siteRoot :: String
  }

servePage :: Config
          -> [(Text, Text)]  -- POST data
          -> [(Text, Text)]  -- GET  data
          -> FilePath        -- input file
          -> IO Text
servePage Config{..} post get file =
  withCreateProcess ((proc mary ["find", "--user", user, siteRoot, file])
                     { std_in  = CreatePipe
                     , std_out = CreatePipe
                     }) $ \ (Just hin) (Just hmaryfind) _ _ -> do
    B.hPut hin $ phpify post
    B.hPut hin $ phpify get
    hClose hin
    withCreateProcess ((proc pandoc ["--data-dir=data", "--standalone", "-f", "markdown", "--filter=marypandoc", "-t", "html", "--template", "templates/mary.html5"])
                     { std_in  = UseHandle hmaryfind
                     , std_out = CreatePipe
                     }) $ \ _ (Just hpandoc) _ _ ->
      TIO.hGetContents hpandoc
  where
    encString = PHPSessionValueString . B.fromStrict . encodeUtf8
    phpify a = encodePHPSessionValue $ PHPSessionValueArray $
                 fmap (encString *** encString) a
