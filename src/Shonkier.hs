module Shonkier where

import Control.Monad.Trans
import qualified Data.Map as Map

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Shonkier.Pretty ()
import Shonkier.Semantics
import Shonkier.ShonkierJS
import Shonkier.Syntax

onShonkierModule :: (Term -> ShonkierT IO a)
                 -> String -> IO a
onShonkierModule action filename = flip evalShonkierT emptyShonkierState $ do
  mod@(is, ps) <- forceImportModule filename
  case [ m | ("main", Right m) <- ps ] of
    [([],body)] -> action body
    _ -> error "not exactly one main function"

interpretShonkier :: String -> IO ()
interpretShonkier = onShonkierModule $ \ body ->
  eval (Map.empty, body) >>= \case
    Value v -> liftIO $ putDoc $ pretty v <> line
    Request (r,_) fr -> error $ "unhandled request " ++ r

-- no support for imports here yet!
compileShonkier :: String -> IO Text
compileShonkier = onShonkierModule $ \ body -> liftIO $ do
  -- Couldn't figure how to import in node so I just concat the
  -- whole interpreter defined 'Shonkier.js' on top
  interpreter <- TIO.readFile "./src/Shonkier/Shonkier.js"
  let header txt = T.concat ["\n/***** "
                            , txt
                            , " "
                            , T.pack (replicate (72 - 10 - T.length txt) '*')
                            , "*/\n\n"]
  pure $ T.concat $ [interpreter, header "Global env"]
                  ++ [] -- jsGlobalEnv ps
                  ++ [header "Main", jsMain]
