module Shonkier.Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Ratio
import qualified Data.Text as T

import Shonkier.Syntax

program :: Parser Program
program = id <$ skipSpace
            <*> many ((,) <$> identifier <*> (Left <$> decl <|> Right <$>defn) <* skipSpace)
             <* endOfInput

decl :: Parser [[String]]
decl = id <$ punc '('
          <*> sep (punc ',') (sep skipSpace atom)
          <* punc ')'  <* char ':'

defn :: Parser Clause
defn = (,) <$ punc '('
          <*> sep (punc ',') pcomputation
           <* punc ')'  <* arrow <* skipSpace
          <*> term

punc :: Char -> Parser ()
punc c = () <$ skipSpace <* char c <* skipSpace

sep :: Parser () -> Parser x -> Parser [x]
sep s p = (:) <$> p <*> many (id <$ s <*> p) <|> pure []

term :: Parser Term
term = weeTerm >>= moreTerm

atom :: Parser String
atom = id <$ char '\'' <*> some (satisfy isAlphaNum)

identifier :: Parser String
identifier = some (satisfy isAlphaNum)

arrow :: Parser ()
arrow = () <$ char '-' <* char '>'

literal :: Parser Literal
literal = stringlit <|> numlit

stringlit :: Parser Literal
stringlit = do
  k <- option "" identifier
  let end = '"':k
  let delim []       _ = Nothing
      delim (d : ds) c
        | c == d    = Just ds
        | otherwise = Just end
  String k <$  char '"'
           <*> (T.dropEnd (length end) <$> scan end delim)

  

data NumExtension
  = Dot   String
  | Slash String
  | None

numlit :: Parser Literal
numlit = do
  n   <- read <$> some (satisfy isDigit)
  ext <- choice [ Dot  <$ char '.' <*> some (satisfy isDigit)
                , Slash <$ char '/' <*> some (satisfy isDigit)
                , pure None
                ]
  pure $ Num $ case ext of
    Dot   rs -> (n % 1) + (read rs % read ('1' : ('0' <$ rs)))
    Slash rs -> n % read rs
    None     -> n % 1

weeTerm :: Parser Term
weeTerm =
  Atom <$>  atom
  <|>
  Lit <$> literal
  <|>
  Var <$> identifier
  <|>
  flip (foldr Cell) <$ char '['
    <*> many (skipSpace *> term) <* skipSpace
    <*> (id <$ char '|' <* skipSpace <*> term <|> pure (Atom ""))
    <* skipSpace <* char ']'
  <|>
  Fun [] <$ char '{' <* skipSpace <*> sep (punc '|') clause <* skipSpace <* char '}'

moreTerm :: Term -> Parser Term
moreTerm t = ((App t <$ punc '(' <*> sep (punc ',') term <* punc ')') >>= moreTerm)
           <|> pure t


clause :: Parser Clause
clause = (,) <$> sep (punc ',') pcomputation <* skipSpace <* arrow <* skipSpace
             <*> term
  <|> (,) [] <$> term

pcomputation :: Parser PComputation
pcomputation
  =   PValue <$> pvalue
  <|> id <$ char '{' <* skipSpace <*>
      (    PThunk <$> identifier
       <|> PRequest <$> ((,) <$> atom <* punc '(' <*> sep (punc ',') pvalue <* punc ')')
           <* arrow <* skipSpace <*> identifier
      ) <* skipSpace <* char '}'

pvalue :: Parser PValue
pvalue =
  PAtom <$ char '\'' <*> some (satisfy isAlphaNum)
  <|>
  PBind <$> some (satisfy isAlphaNum)
  <|>
  flip (foldr PCell) <$ char '[' <*> many (skipSpace *> pvalue) <* skipSpace
          <*> (id <$ char '|' <* skipSpace <*> pvalue <|> pure (PAtom ""))
           <* skipSpace <* char ']'
