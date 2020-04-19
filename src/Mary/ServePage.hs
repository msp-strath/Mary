-- | Hello!

module Mary.ServePage where

import Control.Arrow

import Data.List as L

import qualified Data.ByteString.Lazy as B
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO as TIO

import Data.PHPSession

import Network.URI.Encode as URI

import System.Process
import System.IO

data Config = Config
  { mary   :: FilePath
  , pandoc :: FilePath
  , user   :: String
  , siteRoot :: String
  }

servePage :: Config
          -> [(Text, Text)]  -- POST data (URL-encoded)
          -> [(Text, Text)]  -- GET  data (URL-encoded)
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
    withCreateProcess ((proc pandoc ["--data-dir=data", "--standalone", "-f", "markdown", "--filter", "marypandoc.sh", "-t", "html", "--template", "templates/mary.html5"])
                     { std_in  = UseHandle hmaryfind
                     , std_out = CreatePipe
                     }) $ \ _ (Just hpandoc) _ _ ->
      TIO.hGetContents hpandoc
  where
    encString = PHPSessionValueString . B.fromStrict . encodeUtf8 . URI.decodeText
    phpify a = encodePHPSessionValue $ PHPSessionValueArray $
                 fmap (encString *** encString) a

parseRequests :: Text -> [(Text, Text)]
parseRequests x = L.concatMap pairs $ splitOn "&" x
  where pairs s = case splitOn "=" s of
                    [a,b] -> [(a, b)]
                    [a]   -> [(a, "")]
                    _     -> []
