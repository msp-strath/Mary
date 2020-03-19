{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Mary.Find
       ( maryFind
       ) where

import Data.PHPSession
import Data.ByteString.Lazy as B (getContents, toStrict)
import Data.HashMap.Strict as H
import Data.List as L
import Data.Yaml as Y
import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Encoding
import System.FilePath
import System.Directory
import System.Process

postText :: PHPSessionValue -> Y.Value
postText (PHPSessionValueArray kvs) = object
  [ (decodeUtf8 (B.toStrict k) .= decodeUtf8 (B.toStrict v))
  | (PHPSessionValueString k, PHPSessionValueString v) <- kvs
  ]
postText _ = Y.Null

outputMeta :: Value -> IO ()
outputMeta d@(Y.Object o) | not (H.null o) = TIO.putStr (decodeUtf8 (Y.encode d))
outputMeta _                               =  return ()

maryFind :: FilePath   -- what is the site root?
         -> String     -- username
         -> FilePath   -- page
         -> IO ()      -- PHP $POST $GET -[mary find user site page>- --- YAML ... markdown
maryFind user sitesRoot page = do
  pbs <- B.getContents
  -- the inputs are serialised PHP objects representing post and get data, respectively
  let mpost = L.unfoldr decodePartialPHPSessionValue pbs
  let (postData, getData) = case mpost of
        (p : g : _) -> (postText p, postText g)
        _           -> (Y.Null, Y.Null)
  -- now we have made them YAML objects
  case splitDirectories page of
    (site : _) -> do
      dirEx <- doesDirectoryExist (sitesRoot </> site)
      if not dirEx || not (isValid page)
        then TIO.putStrLn $ T.concat ["Mary cannot find site ", pack site, "!"]
      else do
        -- have we been asked to update the site? if so, git pull!
        case parseMaybe (withObject "get data" $ \ x -> x .:? "pull") getData of
          Just (Just (_ :: Text)) -> callProcess (sitesRoot </> "gitpullsite") [site]
          _ -> return ()
        -- now let us serve up (for pandoc) the YAML data, then the markdown page
        let sitePage = sitesRoot </> page
        fileEx <- doesFileExist sitePage
        if not fileEx
           then TIO.putStrLn $ T.concat ["Mary cannot find page ", pack page, "!"]
        else do
            -- serve the metadata
            TIO.putStrLn "---"
            TIO.putStr (decodeUtf8 (Y.encode (object ["user" .= T.pack user])))
            -- TIO.putStr (decodeUtf8 (Y.encode (object ["sitesRoot" .= T.pack sitesRoot])))
            outputMeta postData
            outputMeta getData
            TIO.putStrLn "..."
            TIO.putStrLn ""
            -- serve the markdown page
            TIO.readFile sitePage >>= TIO.putStr
    _ -> TIO.putStrLn "Mary does not know which page you want!"
