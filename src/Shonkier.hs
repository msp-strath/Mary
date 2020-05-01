module Shonkier where

import System.FilePath
import System.IO.Error
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
interpretShonkier = onShonkierModule $ \ _ gl body -> go gl (shonkier gl body) where
  go _ (Value v) = putDoc $ pretty v <> line
  go gamma (Request ("read", [f]) k) = case value2path f of
    Nothing -> go gamma (resumeShonkier gamma k abort)
    Just f -> do
      tryIOError (TIO.readFile f) >>= \case
        Right t  -> go gamma (resumeShonkier gamma k (use (VString "" t)))
        Left _   -> go gamma (resumeShonkier gamma k abort)
  go gamma (Request ("write", [f,VString _ t]) k) = case value2path f of
    Nothing -> go gamma (resumeShonkier gamma k abort)
    Just f -> do
      tryIOError (TIO.writeFile f t) >>= \case
        Right _ -> go gamma (resumeShonkier gamma k (use VNil))
        Left _  -> go gamma (resumeShonkier gamma k abort)
  go _ r@Request{} = do
      let r' = renderStrict $ layoutPretty defaultLayoutOptions $ pretty r
      error $ "unhandled request " ++ T.unpack r'

value2path :: Value -> Maybe FilePath
value2path v = makeValid <$> go v where
  go (VString _ t) = pure (T.unpack t)
  go VNil = pure ""
  go (VCell s t) = (</>) <$> go s <*> go t
  go _ = Nothing

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
