module Mary.Pandoc where

import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Newtype

import Data.Attoparsec.Text
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T

import Text.Pandoc.Builder
import Text.Pandoc.Walk

import Shonkier.Parser
import Shonkier.Semantics
import Shonkier.ShonkierJS
import Shonkier.Syntax

process :: Pandoc -> IO Pandoc
process doc0 = return doc3 where
  (doc1, defns) = runWriter (walkM snarfMaryDef doc0)
  ps = concat [p | ds <- defns, let Right p = parseOnly program ds]
  env = mkGlobalEnv ps
  doc2 = walk (evalMary env) doc1

  h1 :: Block -> Maybe Inlines
  h1 (Header 1 _ is) = Just (fromList is)
  h1 _ = Nothing

  doc3 = setTitle (fromMaybe "Title TBA" (ala' First query h1 doc0))
       . setMeta "jsGlobalEnv" (jsGlobalEnv ps)
       $ doc2

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
