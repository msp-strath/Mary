{-# LANGUAGE OverloadedStrings #-}

module Shonkier.Parser.Examples where

import Shonkier.Parser
import Shonkier.Syntax

import Data.Attoparsec.Text
import Data.Text

getMeATerm :: Text -> Term
getMeATerm txt = case parseOnly (term <* endOfInput) txt of
  Left err -> error err
  Right t  -> t

mapT :: Term
mapT = getMeATerm
   "{ f, []     -> []                   \
  \ | f, [x|xs] -> [f(x)|map(f, xs)]   \
  \ }"
