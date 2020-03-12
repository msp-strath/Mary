module Mary.Pandoc where

import Control.Monad.Writer (Writer, runWriter, tell)

import Data.Attoparsec.Text
import qualified Data.Text as T

import Text.Pandoc.Definition
import Text.Pandoc.Walk

import Shonkier.Parser
import Shonkier.Semantics
import Shonkier.Syntax

process :: Pandoc -> IO Pandoc
process doc0 = return doc2 where
  (doc1, defns) = runWriter (walkM snarfMaryDef doc0)
  ps = concat [p | ds <- defns, let Right p = parseOnly program ds]
  env = mkGlobalEnv ps
  doc2 = walk (evalMary env) doc1

snarfMaryDef :: Block -> Writer [T.Text] Block
snarfMaryDef (CodeBlock (_, cs, _) p) | elem "mary-def" cs = Null <$ tell [p]
snarfMaryDef b = return b

evalMary :: Env -> Block -> Block
evalMary env (CodeBlock (_, cs, _) e) | elem "mary" cs =
  case parseOnly (term <* endOfInput) e of
    Left _ -> Null
    Right t -> case shonkier env t of
      Value v -> fromValue v
      _ -> Null
evalMary _ b = b

--

instance FromValue Block where
  fromValue (VCell (VAtom "Para") is) = Para (fromValue is)
  fromValue _ = Null

instance FromValue Inline where
  fromValue (VLit (String _ t)) = Str t
  fromValue _ = Space
