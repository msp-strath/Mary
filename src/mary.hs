module Main where

import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text.IO as TIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import System.Environment

import Text.Pandoc.JSON

import Data.Bwd
import Shonkier.Examples
import Shonkier.Pandoc
import Shonkier.Parser
import Shonkier.Parser.Examples ()
import Shonkier.Pretty ()
import Shonkier.Pretty.Examples ()
import Shonkier.Semantics

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ["-pandoc"]             -> toJSONFilter process
    ["-shonkier", filename] -> interpretShonkier filename
    _ -> TIO.putStr "# mary says\nI don't know what you're on about.\n\n"

interpretShonkier :: String -> IO ()
interpretShonkier filename = do
  file <- TIO.readFile filename
  let ps = getMeAProgram file
  let env = primEnv <> mkEnv ps
  case [ m | ("main", Right m) <- ps ] of
    [([],body)] ->
      case eval Nil (env, body) of
        Value v -> putDoc $ pretty v <> line
        Request (r,_) fr -> error $ "unhandled request " ++ r
    _ -> error "not exactly one main function"
