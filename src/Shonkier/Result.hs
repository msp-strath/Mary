module Shonkier.Result where

import Shonkier.Dot()
import Shonkier.Pandoc()
import Shonkier.Value

import Dot.Types
import Text.Pandoc.Definition

data Result
  = ResultPandoc Block
  | ResultDot DotGraph

instance FromValue Result where
  fromValue (VCell (VAtom "DOT") b) = ResultDot <$> fromValue b
  fromValue v = ResultPandoc <$> fromValue v
