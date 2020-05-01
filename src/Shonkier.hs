module Shonkier where

import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc (line, layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text

import Shonkier.Pretty
import Shonkier.Value
import Shonkier.Import
import Shonkier.Semantics
import Shonkier.ShonkierJS
import Shonkier.Syntax

onShonkierModule :: (Module -> Env -> Term -> IO a)
                 -> FilePath -> IO a
onShonkierModule action filename = do
  (mod@(is, ps), gl) <- importToplevelModule filename
  case [ m | ("main", Right m) <- ps ] of
    [([] :-> [Nothing :?> body])] -> action mod (gl, mempty) body
    _ -> error "not exactly one simple main function"

interpretShonkier :: FilePath -> IO ()
interpretShonkier = onShonkierModule $ \ _ gl body ->
  case shonkier gl body of
    Value v -> putDoc $ pretty v <> line
    r@Request{} -> do
      let r' = renderStrict $ layoutPretty defaultLayoutOptions $ pretty r
      error $ "unhandled request " ++ T.unpack r'

-- no support for imports here yet!
compileShonkier :: FilePath -> FilePath -> IO Text
compileShonkier shonkierjs fp = (`onShonkierModule` fp) $ \ _ (env,inputs) body -> do
  -- Couldn't figure how to import in node so I just concat the
  -- whole interpreter defined 'Shonkier.js' on top
  interpreter <- TIO.readFile shonkierjs
  let header txt = T.concat ["\n/***** "
                            , txt
                            , " "
                            , T.pack (replicate (72 - 10 - T.length txt) '*')
                            , "*/\n\n"]
  pure $ T.concat $ [interpreter]
                  ++ (header "Global env"):(jsGlobalEnv env)
                  ++ (header "Form data"):(jsInputs inputs)
                  ++ (header "Main"):(jsRun body)
