module Shonkier where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Shonkier.Pretty ()
import Shonkier.Value
import Shonkier.Import
import Shonkier.Semantics
import Shonkier.ShonkierJS
import Shonkier.Syntax

onShonkierModule :: (Module -> GlobalEnv -> Term -> IO a)
                 -> FilePath -> IO a
onShonkierModule action filename = do
  (mod@(is, ps), gl) <- importToplevelModule filename
  case [ m | ("main", Right m) <- ps ] of
    [([],body)] -> action mod gl body
    _ -> error "not exactly one main function"

interpretShonkier :: FilePath -> IO ()
interpretShonkier = onShonkierModule $ \ _ gl body ->
  case shonkier gl body of
    Value v -> putDoc $ pretty v <> line
    Request (r,_) fr -> error $ "unhandled request " ++ r

-- no support for imports here yet!
compileShonkier :: FilePath -> FilePath -> IO Text
compileShonkier shonkierjs fp = (`onShonkierModule` fp) $ \ _ env body -> do
  -- Couldn't figure how to import in node so I just concat the
  -- whole interpreter defined 'Shonkier.js' on top
  interpreter <- TIO.readFile shonkierjs
  let header txt = T.concat ["\n/***** "
                            , txt
                            , " "
                            , T.pack (replicate (72 - 10 - T.length txt) '*')
                            , "*/\n\n"]
  pure $ T.concat $ [interpreter, header "Global env"]
                  ++ jsGlobalEnv env
                  ++ [header "Main", jsMain fp]
