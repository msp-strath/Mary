module Mary.Parser where

import Control.Applicative

import Data.Attoparsec.Text hiding (skipSpace)
import qualified Data.Text as T

import Shonkier.Parser
import Mary.Syntax


maryData :: Parser [DataDirective]
maryData = many $
  DataDirective
    <$ skipSpace
    <*> (StoreName . T.pack <$> identifier)
    <* skipSpace
    <*> readWriteStatus
    <* skipSpace
    <*> topTerm
    <*> optional (id <$ punc "@" <*> topTerm)

readWriteStatus :: Parser ReadWriteStatus
readWriteStatus =
  ReadWrite <$ string "<->"
  <|> ReadOnly <$ string "<-"
  <|> WriteOnly <$ string "->"
