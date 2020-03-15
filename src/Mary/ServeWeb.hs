{-# LANGUAGE OverloadedStrings #-}

module Mary.ServeWeb where

import Data.PHPSession
import Data.ByteString.Lazy as B (getContents, toStrict)
import Data.List as L
import Data.Yaml as Y
import Data.Text as T
import Data.Text.Encoding

postText :: PHPSessionValue -> Y.Value
postText (PHPSessionValueArray kvs) = object
  [ (decodeUtf8 (B.toStrict k) .= decodeUtf8 (B.toStrict v))
  | (PHPSessionValueString k, PHPSessionValueString v) <- kvs
  ]
postText _ = Y.Null

serveWeb :: String   -- username
         -> String   -- page
         -> IO Text  -- expecting get and post data to be serialised on stdin
serveWeb user page = do
  pbs <- B.getContents
  let mpost = L.unfoldr decodePartialPHPSessionValue pbs
  let (postData, getData) = case mpost of
        (p : g : _) -> (postText p, postText g)
        _ -> (Y.Null, Y.Null)
  return . T.concat $
    [ "Hi!\n"
    , decodeUtf8 (Y.encode (object ["user" .= T.pack user]))
    , decodeUtf8 (Y.encode getData)
    , decodeUtf8 (Y.encode postData)
    ]