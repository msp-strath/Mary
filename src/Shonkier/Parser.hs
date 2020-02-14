module Shonkier.Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.List (sortBy, groupBy, nub)
import Data.Function
import Data.Either
import Data.Map (singleton)
import Data.Foldable
import Control.Arrow

import Shonkier.Syntax
import Shonkier.Semantics

type Program = [(String, Either [[String]] Clause)]

mkEnv :: Program -> Env
mkEnv ls0 = env
  where
  ls1 = sortBy (compare `on` (id *** isRight)) ls0
  lss = groupBy ((==) `on` fst) ls1
  env = fold
          [ singleton f $
             VFun [] env
               (map nub (foldr padCat [] [hs | (_, Left hs) <- grp]))
               [cl | (_, Right cl) <- grp]
          | grp@((f, _) : _) <- lss
          ]

padCat :: Eq a => [[a]] -> [[a]] -> [[a]]
padCat [] hs = hs
padCat hs [] = hs
padCat (a : as) (b : bs) = (a ++ b) : padCat as bs


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
           <* punc ')'  <* char '-' <* char '>' <* skipSpace
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

weeTerm :: Parser Term
weeTerm =
  Atom <$>  atom
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
clause = (,) <$> sep (punc ',') pcomputation <* skipSpace <* char '-' <* char '>' <* skipSpace
             <*> term
  <|> (,) [] <$> term

pcomputation :: Parser PComputation
pcomputation
  =   PValue <$> pvalue
  <|> id <$ char '<' <* skipSpace <*>
      (    PThunk <$> identifier
       <|> PRequest <$> ((,) <$> atom <* punc '(' <*> sep (punc ',') pvalue <* punc ')')
           <* char '-' <* char '>' <* skipSpace <*> identifier
      ) <* skipSpace <* char '>'

pvalue :: Parser PValue
pvalue =
  PAtom <$ char '\'' <*> some (satisfy isAlphaNum)
  <|>
  PBind <$> some (satisfy isAlphaNum)
  <|>
  flip (foldr PCell) <$ char '[' <*> many (skipSpace *> pvalue) <* skipSpace
          <*> (id <$ char '|' <* skipSpace <*> pvalue <|> pure (PAtom ""))
           <* skipSpace <* char ']'
