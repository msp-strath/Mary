{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Pandoc.JSON
import qualified Data.Text as T
import Data.Text.IO as TIO
import System.Environment
import Control.Monad.Writer
import Data.Attoparsec.Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Data.Bwd
import Shonkier.Examples
import Shonkier.Parser
import Shonkier.Parser.Examples ()
import Shonkier.Pretty ()
import Shonkier.Pretty.Examples ()
import Shonkier.Semantics
import Shonkier.Syntax

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
  let Right ps = parseOnly program file
  let env = primEnv <> mkEnv ps
  case [ m | ("main", Right m) <- ps ] of
    [([],body)] ->
      case eval Nil (env, body) of
        Value v -> putDoc $ pretty v <> line
        Request (r,_) fr -> error $ "unhandled request " ++ r
    _ -> error "not exactly one main function"

process :: Pandoc -> IO Pandoc
process doc0 = return doc2 where
  (doc1, defns) = runWriter (walkM snarfMaryDef doc0)
  ps = concat [p | ds <- defns, let Right p = parseOnly program ds]
  env = mkEnv ps
  doc2 = walk (evalMary env) doc1

snarfMaryDef :: Block -> Writer [T.Text] Block
snarfMaryDef (CodeBlock (_, cs, _) p) | elem "mary-def" cs = Null <$ tell [p]
snarfMaryDef b = return b

evalMary :: Env -> Block -> Block
evalMary env (CodeBlock (_, cs, _) e) | elem "mary" cs =
  case parseOnly (term <* endOfInput) e of
    Left _ -> Null
    Right t -> case eval Nil (env, t) of
      Value v -> fromValue v
      _ -> Null
evalMary _ b = b

-- MOVE THIS!
class FromValue t where
  fromValue :: Value -> t

instance FromValue Block where
  fromValue (VCell (VAtom "Para") is) = Para (fromValue is)
  fromValue _ = Null

instance FromValue t => FromValue [t] where
  fromValue (VCell t ts) = fromValue t : fromValue ts
  fromValue _ = []

instance FromValue Inline where
  fromValue (VLit (String _ t)) = Str t
  fromValue _ = Space
