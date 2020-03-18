{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Mary.ServeWeb where

import Data.PHPSession
import Data.ByteString.Lazy as B (getContents, toStrict)
import Data.List as L
import Data.Yaml as Y
import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Encoding
import System.FilePath
import System.Directory
import System.Process

import Mary.ServePage -- for now

postText :: PHPSessionValue -> Y.Value
postText (PHPSessionValueArray kvs) = object
  [ (decodeUtf8 (B.toStrict k) .= decodeUtf8 (B.toStrict v))
  | (PHPSessionValueString k, PHPSessionValueString v) <- kvs
  ]
postText _ = Y.Null

serveWeb :: Config     -- where are mary and pandoc?
         -> FilePath   -- what is the site root?
         -> String     -- username
         -> FilePath   -- page
         -> IO Text    -- expecting get and post data to be serialised on stdin
serveWeb config sitesRoot user page = do
  pbs <- B.getContents
  let mpost = L.unfoldr decodePartialPHPSessionValue pbs
  let (postData, getData) = case mpost of
        (p : g : _) -> (postText p, postText g)
        _ -> (Y.Null, Y.Null)
  case splitDirectories page of
    (site : _) -> do
      dirEx <- doesDirectoryExist (sitesRoot </> site)
      if not dirEx || not (isValid page)
        then return $ T.concat ["Mary cannot find site ", pack site, "!"]
        else do
          case parseMaybe (withObject "get data" $ \ x -> x .:? "pull") getData of
            Just (Just (_ :: Text)) -> callProcess "./gitpullsite" [site]
            _ -> return ()
          let sitePage = sitesRoot </> page
          fileEx <- doesFileExist sitePage
          if not fileEx then return $ T.concat ["Mary cannot find page ", pack page, "!"]
            else do
              TIO.putStrLn "Hello!"
              servePage config sitePage
    _ -> return "Mary does not know which page you want!"
