module Shonkier where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Shonkier.Parser
import Shonkier.Pretty ()
import Shonkier.Semantics
import Shonkier.ShonkierJS
import Shonkier.Syntax

onShonkierProgram :: (Program -> Term -> IO a) -> String -> IO a
onShonkierProgram action filename = do
  file <- TIO.readFile filename
  let ps = getMeAProgram file
  case [ m | ("main", Right m) <- ps ] of
    [([],body)] -> action ps body
    _ -> error "not exactly one main function"

interpretShonkier :: String -> IO ()
interpretShonkier = onShonkierProgram $ \ ps body ->
  case shonkier (mkGlobalEnv ps) body of
    Value v -> putDoc $ pretty v <> line
    Request (r,_) fr -> error $ "unhandled request " ++ r

compileShonkier :: String -> IO Text
compileShonkier = onShonkierProgram $ \ ps _ -> do
  -- Couldn't figure how to import in node so I just concat the
  -- whol interpreter defined 'Shonkier.js' on top
  interpreter <- TIO.readFile "./src/Shonkier/Shonkier.js"
  let header txt = T.concat ["\n/***** "
                            , txt
                            , " "
                            , T.pack (replicate (72 - 10 - T.length txt) '*')
                            , "*/\n\n"]
  pure $ T.concat $ [interpreter, header "Global env"]
                  ++ jsGlobalEnv ps
                  ++ [header "Main", jsMain]
