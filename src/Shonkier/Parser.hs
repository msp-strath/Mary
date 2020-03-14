module Shonkier.Parser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Data.Ratio
import Data.Text(Text)
import qualified Data.Text as T

import Shonkier.Syntax

program :: Parser Program
program = id <$ skipSpace
            <*> many ((,) <$> identifier <*> (Left <$> decl <|> Right <$>defn) <* skipSpace)
             <* endOfInput

decl :: Parser [[String]]
decl = tupleOf (sep skipSpace atom) <* char ':'

defn :: Parser Clause
defn = (,) <$> tupleOf pcomputation
           <* arrow <* skipSpace <*> term

punc :: Char -> Parser ()
punc c = () <$ skipSpace <* char c <* skipSpace

sep :: Parser () -> Parser x -> Parser [x]
sep s p = (:) <$> p <*> many (id <$ s <*> p) <|> pure []

term :: Parser Term
term = weeTerm >>= moreTerm

atom :: Parser String
atom = id <$ char '\'' <*> some (satisfy isAlphaNum)

identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

arrow :: Parser ()
arrow = () <$ char '-' <* char '>'

literal :: Parser Literal
literal = stringlit <|> charlit <|> numlit

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

charlit :: Parser Literal
charlit = do
  k <- option "" identifier
  let end = T.pack $ '\'':k
  Char k <$ char '\'' <*> anyChar <* string end

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

listOf :: Parser a -> a -> Parser ([a], a)
listOf p nil = (,) <$ char '['
      <*> many (skipSpace *> p) <* skipSpace
      <*> (id <$ char '|' <* skipSpace <*> p <|> pure nil)
      <* skipSpace <* char ']'

tupleOf :: Parser a -> Parser [a]
tupleOf p = id <$ punc '(' <*> sep (punc ',') p <* punc ')'

weeTerm :: Parser Term
weeTerm =
  Atom <$> atom
  <|>
  Lit <$> literal
  <|>
  Var <$> identifier
  <|>
  uncurry (flip $ foldr Cell) <$> listOf term (Atom "")
  <|>
  Fun [] <$ char '{' <* skipSpace <*> sep (punc '|') clause <* skipSpace <* char '}'

moreTerm :: Term -> Parser Term
moreTerm t = (App t <$> tupleOf term >>= moreTerm)
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
       <|> PRequest <$> ((,) <$> atom <*> tupleOf pvalue)
           <* arrow <* skipSpace <*> (Just <$> identifier <|> Nothing <$ char '_')
      ) <* skipSpace <* char '}'

pvar :: Parser PValue
pvar = do
  var <- identifier
  choice [ PAs var <$ char '@' <*> pvalue
         , pure (PBind var)
         ]

pvalue :: Parser PValue
pvalue = choice
  [ PLit <$> literal
  , PAtom <$> atom
  , pvar
  , PWild <$ char '_'
  , uncurry (flip $ foldr PCell) <$> listOf pvalue (PAtom "")
  ]

getMeAProgram :: Text -> Program
getMeAProgram txt = case parseOnly program txt of
  Left err -> error err
  Right t  -> t

getMeATerm :: Text -> Term
getMeATerm txt = case parseOnly (term <* endOfInput) txt of
  Left err -> error err
  Right t  -> t
