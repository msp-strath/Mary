{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Mary.Find
       ( maryFind
       ) where

import Data.PHPSession
import Data.ByteString.Lazy as B (getContents, toStrict)
import Data.HashMap.Strict as H
import Data.List as L
import Data.Yaml as Y
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text as T
import Data.Text.IO as TIO
import Data.Text.Encoding

import Network.URI.Encode as URI

import System.FilePath
import System.Directory
import System.Process

postText :: Text -> PHPSessionValue -> Y.Value
postText prefix (PHPSessionValueArray kvs) = object
  [ (key .= value)
  | (PHPSessionValueString k, PHPSessionValueString v) <- kvs
  , let key = prefix <> "_" <> decodeUtf8 (B.toStrict k)
  , let value = "`" <> (URI.encodeText $ decodeUtf8 $ B.toStrict v) <> "`{=html}"
  ]
postText _ _ = Y.Null

outputMeta :: Value -> IO ()
outputMeta d@(Y.Object o) | not (H.null o) = TIO.putStr (decodeUtf8 (Y.encode d))
outputMeta _                               =  return ()

maryFind :: FilePath   -- what is the site root?
         -> String   -- what is the base URL?
         -> Maybe String     -- username
         -> FilePath   -- page
         -> IO ()      -- PHP $POST $GET -[mary find user site page>- --- YAML ... markdown
maryFind sitesRoot baseURL user page = do
  pbs <- B.getContents
  -- the inputs are serialised PHP objects representing post and get data, respectively
  let mpost = L.unfoldr decodePartialPHPSessionValue pbs
  let (postData, getData) = case mpost of
        (p : g : _) -> (postText "POST" p, postText "GET" g)
        _           -> (Y.Null, Y.Null)
  -- now we have made them YAML objects
  case splitDirectories page of
    (site : _) -> do
      dirEx <- doesDirectoryExist (sitesRoot </> site)
      if not dirEx || not (isValid page)
        then error $ L.concat ["Mary cannot find site ", site, "!"]
      else do
        -- have we been asked to update the site? if so, git pull!
        case parseMaybe (withObject "get data" $ \ x -> x .:? "GET_pull") getData of
          Just (Just (_ :: Text)) -> callProcess (sitesRoot </> "gitpullsite") [site]
          _ -> return ()
        -- now let us serve up (for pandoc) the YAML data, then the markdown page
        let sitePage = sitesRoot </> page
        fileEx <- doesFileExist sitePage
        if not fileEx then do
          error $ L.concat ["Mary cannot find page ", page, "!"]
        else do
            -- serve the metadata
            TIO.putStrLn "---"
            case user of
              Nothing -> pure ()
              Just u -> TIO.putStr (decodeUtf8 (Y.encode (object ["user" .= T.pack u])))
            TIO.putStr (decodeUtf8 (Y.encode (object ["baseURL" .= T.pack baseURL])))
            TIO.putStr (decodeUtf8 (Y.encode (object ["sitesRoot" .= T.pack sitesRoot])))

            outputMeta postData
            outputMeta getData
            TIO.putStrLn "..."
            TIO.putStrLn ""
            -- serve the markdown page
            TIO.readFile sitePage >>= TIO.putStr
    _ -> error "Mary does not know which page you want!"
