module Shonkier.Parser where

import Control.Applicative

import Data.Attoparsec.Text hiding (skipSpace)
import qualified Data.Attoparsec.Text as Atto
import Data.Char
import Data.Functor (void)
import Data.Ratio
import Data.Text(Text)
import qualified Data.Text as T

import Debug.Trace

import Shonkier.Syntax

skipSpace :: Parser ()
skipSpace = comments

module_ :: Parser RawModule
module_ = (,) <$> many (import_ <* skipSpace) <*> program

import_ :: Parser Import
import_ = do
  () <$ string "import"
  skipSpace
  String _ fp <- stringlit
  skipSpace
  alias <- choice [ Just <$ string "as" <* skipSpace <*> identifier
                  , pure Nothing
                  ]
  pure (T.unpack fp, alias)

program :: Parser RawProgram
program = id <$ skipSpace
            <*> many ((,) <$> identifier <*> (Left <$> decl <|> Right <$>defn) <* skipSpace)
             <* endOfInput

data Comment = Line | Nested deriving (Eq, Show)

comments :: Parser ()
comments = do
  void $ many $ do
    Atto.skipSpace
    choice [ () <$ char '/' <* choice
               [ () <$ char '/' <* trace "entering comments: found //" (scan ([Line], init) delim)
               , () <$ char '*' <* trace "entering comments: found /*" (scan ([Nested], init) delim)
               ]
           ]
  Atto.skipSpace
  where

    -- delimiters
    init = ("//", "/*", "*/")

    -- eating a character
    eats c (s, t, u) = let (p, q, r) = init in (eat p c s, eat q c t, eat r c u)
    eat reset@(e : es) c (d : ds)
      | c == d    = ds
      | c == e    = trace "magic!" es -- The string "**/" is a valid end of comment!
      | otherwise = reset
    eat _ _ _ = error "The IMPOSSIBLE happened while parsing a comment!"

    mayExit ([],_) = trace "exiting comment" Nothing
    mayExit state  = Just state

    -- once the stack is empty, we're done!
    delim ([], _)                    c    = trace "exiting comment" Nothing
    -- dealing with line comments: eat everything up until the end of line
    delim (Line:ctx, _)              '\n' = trace ("found end of //") $ Just (ctx, init)
    delim (ctx@(Line:_), st)         c    = Just (ctx, eats c st)
    delim (ctx, ([], _, _))          c    =
      trace ("found // eating " ++ [c]) $ Just (Line : ctx, eats c init)
    -- dealing with nested comments
    delim (ctx, (_, [], _))          c    =
      trace ("found /* ctx: " ++ show ctx) $ Just (Nested : ctx, eats c init)
    delim (Nested : ctx, (_, _, [])) c    =
      trace ("found */ ctx: " ++ show ctx) $ mayExit (ctx, eats c init)
    -- default: haven't seen anything interesting so keep munching
    delim (ctx, st)                  c    =
      trace ("eating " ++ [c] ++ " ctx: " ++ show ctx) $ Just (ctx, eats c st)

decl :: Parser [[String]]
decl = tupleOf (sep skipSpace atom) <* char ':'

defn :: Parser RawClause
defn = (,) <$> tupleOf pcomputation
           <* arrow <* skipSpace <*> term

punc :: Char -> Parser ()
punc c = () <$ skipSpace <* char c <* skipSpace

sep :: Parser () -> Parser x -> Parser [x]
sep s p = (:) <$> p <*> many (id <$ s <*> p) <|> pure []

term :: Parser RawTerm
term = weeTerm >>= moreTerm

atom :: Parser String
atom = id <$ char '\'' <*> some (satisfy isAlphaNum)

identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

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

listOf :: Parser a -> a -> Parser ([a], a)
listOf p nil = (,) <$ char '['
      <*> many (skipSpace *> p) <* skipSpace
      <*> (id <$ char '|' <* skipSpace <*> p <|> pure nil)
      <* skipSpace <* char ']'

tupleOf :: Parser a -> Parser [a]
tupleOf p = id <$ punc '(' <*> sep (punc ',') p <* punc ')'

variable :: Parser RawVariable
variable = do
  start <- identifier
  next  <- choice [ Just <$ char '.' <*> identifier
                  , pure Nothing ]
  pure $ case next of
    Nothing  -> (Nothing, start)
    Just end -> (Just start, end)

weeTerm :: Parser RawTerm
weeTerm = choice
  [ Atom <$> atom
  , Lit <$> literal
  , Var <$> variable
  , uncurry (flip $ foldr Cell) <$> listOf term (Atom "")
  , Fun [] <$ char '{' <* skipSpace <*> sep (punc '|') clause <* skipSpace <* char '}'
  ]

moreTerm :: RawTerm -> Parser RawTerm
moreTerm t = (App t <$> tupleOf term >>= moreTerm)
           <|> pure t


clause :: Parser RawClause
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

getMeA :: Parser a -> Text -> a
getMeA p txt = case parseOnly p txt of
  Left err -> error err
  Right t  -> t

getMeAModule :: Text -> RawModule
getMeAModule = getMeA module_

getMeAProgram :: Text -> RawProgram
getMeAProgram = getMeA program

getMeATerm :: Text -> RawTerm
getMeATerm = getMeA (term <* endOfInput)
