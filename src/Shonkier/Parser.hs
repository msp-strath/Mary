{-# LANGUAGE MultiWayIf #-}

module Shonkier.Parser where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad

import Data.Attoparsec.Text hiding (skipSpace)
import qualified Data.Attoparsec.Text as Atto
import Data.Char
import Data.Functor (void)
import Data.Ratio
import Data.Text(Text)
import qualified Data.Text as T

import Shonkier.Syntax

-- We can insert comments anywhere we may insert space
skipSpace :: Parser ()
skipSpace = comments

module_ :: Parser RawModule
module_ = (,) <$> many (import_ <* skipSpace) <*> program

import_ :: Parser Import
import_ = do
  () <$ string "import"
  skipSpace
  (_, [], fp) <- spliceOf (choice [])
  skipSpace
  alias <- choice [ Just <$ string "as" <* skipSpace <*> identifier
                  , pure Nothing
                  ]
  pure (T.unpack fp, alias)

program :: Parser RawProgram
program = id <$ skipSpace
            <*> many ((,) <$> identifier <*> (Left <$> decl <|> Right <$> defn) <* skipSpace)
             <* endOfInput

data Comment = Line | Nested deriving (Eq, Show)

comments :: Parser ()
comments = do
  Atto.skipSpace
  -- we may have many space-separated comments
  void $ many $ do
    () <$ char '/' <* choice [ char '/' <* match Line
                             , char '*' <* match Nested
                             ]
    Atto.skipSpace
  where

    -- kickstarting the process: scanning the file until we have closed
    -- the comment delimiter @style@ and all potentially nested comments.
    match style = scan ([style], init) (uncurry delim)

    -- delimiters
    init = ("//", "/*", "*/")

    -- eating a character
    eats c (s, t, u) = let (p, q, r) = init in (eat p c s, eat q c t, eat r c u)

    -- @eat reset c state@ tries to eat the character @c@ from the @state@.
    -- If it cannot, we had a mismatch & need to restart matching with @reset@.
    eat reset@(e : es) c (d : ds)
      -- success
      | c == d    = ds
      -- surprise: we failed but could make progress along @reset@!
      -- See e.g. that the string "**/" is a valid end of comment despite
      -- "**" not being one. We fail finding the end-of-comment delimiter
      -- on the second '*' but still make enough progress to succeed after
      -- only reading one additional '/'.
      -- We can afford to only have this check because all of our delimiters
      -- have length 2. Otherwise we would need a more complex DFA to perform
      -- this kind of backtracking.
      | c == e    = es
      -- catchall: hard fail, start from the very beginning
      | otherwise = reset
      -- If the @state@ is empty we should have been done already.
      -- None of the potential @reset@ are empty so this last case
      -- can never happen.
    eat _ _ _ = error "The IMPOSSIBLE happened while parsing a comment!"

    -- @delim stk state c@ checks whether eating @c@ in the state @state@
    -- is enough to have closed all of the opened delimiters stored in the
    -- stack @stk@.
    -- The state, a triple of strings, is obscure enough that I have declared
    -- pattern synonyms to clarify what the different configuations mean. They
    -- are below because we may only declare pattern synonyms at the top-level.

    -- if the stack has just been emptied, we're done!
    delim []             _           c    = Nothing
    -- line comments: eat everything up until the end of line
    delim (Line : stk)   _           '\n' = Just (stk, init)
    delim stk@(Line : _) st          c    = Just (stk, eats c st)
    delim stk            NewLine{}   c    = Just (Line : stk, eats c init)
    -- nested comments: succeed only once you've closed the last one
    delim stk            NewNested{} c    = Just (Nested : stk, eats c init)
    delim [Nested]       EndNested{} c    = Nothing
    delim (Nested : stk) EndNested{} c    = Just (stk, eats c init)
    -- default: haven't seen anything interesting so keep munching
    delim stk            st          c    = Just (stk, eats c st)

-- ghc only wants invertible patterns...
pattern NewLine   b c = ([], b, c)
pattern NewNested a c = (a, [], c)
pattern EndNested a b = (a, b, [])


decl :: Parser [[String]]
decl = argTuple (sep skipSpace atom) <* skipSpace <* char ':'

defn :: Parser RawClause
defn = (,) <$> argTuple pcomputation <* skipSpace
           <* arrow <* skipSpace <*> term

punc :: Char -> Parser ()
punc c = () <$ skipSpace <* char c <* skipSpace

sep :: Parser () -> Parser x -> Parser [x]
sep s p = (:) <$> p <*> many (id <$ s <*> p) <|> pure []

data Forbidden
  = NoProb
  | NoPrio
  | NoSemi
  | NoMask
  deriving (Show, Ord, Eq, Enum, Bounded)


spaceTerm :: Parser RawTerm
spaceTerm = id <$ skipSpace <*> term <* skipSpace

------------------------------------------------------------------------------
-- NOTA BENE                                                                --
--                                                                          --
-- parsers for terms/patterns must forbid leading/trailing space            --
--                                                                          --
------------------------------------------------------------------------------

term :: Parser RawTerm
term = termBut NoProb

termBut :: Forbidden -> Parser RawTerm
termBut z = weeTerm >>= moreTerm z

weeTerm :: Parser RawTerm
weeTerm = choice
  [ Match <$> pvalue <* skipSpace <* char ':' <* char '=' <* skipSpace <*> termBut NoSemi
  , Atom <$> atom
  , Lit <$> literal
  , (\ (k, t, es) -> String k t es) <$> spliceOf spaceTerm
  , Var <$> variable
  , uncurry (flip $ foldr Cell) <$> listOf term Nil
  , Fun [] <$ char '{' <* skipSpace <*> sep skipSpace clause <* skipSpace <* char '}'
  , id <$ char '(' <*> spaceTerm <* char ')'
  ] where

moreTerm :: Forbidden -> RawTerm -> Parser RawTerm
moreTerm z t = choice
  [ App t <$> argTuple term >>= moreTerm z
  , Mask <$> tmAtom t <* punc '\\' <*> termBut (pred NoMask) >>= moreTerm z
  , Semi t <$ guard (z < NoSemi) <* punc ';' <*> termBut (pred NoSemi) >>= moreTerm z
  , Prio t <$ guard (z < NoPrio) <* punc '%' <*> termBut (pred NoPrio) >>= moreTerm z
  , pure t
  ] where
  tmAtom (Atom a) = pure a
  tmAtom _ = empty

atom :: Parser String
atom = id <$ char '\'' <*> identifier

identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

arrow :: Parser ()
arrow = () <$ char '-' <* char '>'

literal :: Parser Literal
literal = boolit <|> numlit

spliceOf :: Parser a -> Parser (Keyword, [(Text, a)], Text)
spliceOf p = do
  fence <- option "" identifier <* char '"'
  (txt, atxts) <- munchSplice fence
  let (ps, end) = rotate txt atxts
  pure (fence, ps, end)

  where
  rotate txt []                 = ([], txt)
  rotate txt ((a, txt') : rest) = first ((txt, a):) $ rotate txt' rest

  munchSplice fence = do
    let endStr     = '"':fence
    let startSpl   = fence ++ "`"
    let nextEndStr []       c = Nothing
        nextEndStr (d : ds) c
          | c == d    = Just ds
          | c == '"'  = Just fence
          | otherwise = Just endStr
    let nextStartSpl es c
          | [] `elem` es = Nothing
          | otherwise    = Just [ ds | (d : ds) <- startSpl : es, c == d ]
    let delim (a, b) c = (,) <$> nextEndStr a c <*> nextStartSpl b c
    txt <- scan (endStr, []) delim
    if | Just txt' <- T.stripSuffix (T.pack startSpl) txt -> do
          a     <- p
          string (T.pack $ '`':fence)
          mrest <- choice [ Just <$> munchSplice fence
                          , pure Nothing
                          ]
          pure $ case mrest of
            Just (lit, rest) -> (txt', (a, lit):rest)
            Nothing          -> (txt', [(a, "")])
       | Just txt' <- T.stripSuffix (T.pack endStr) txt ->
         pure (txt', [])
       | otherwise -> choice []

boolit :: Parser Literal
boolit = Boolean <$ char '\'' <*> (False <$ char '0' <|> True <$ char '1')

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
      <*> many (spaceMaybeComma *> p) <* spaceMaybeComma
      <*> (id <$ char '|' <* skipSpace <*> p <|> pure nil)
      <* skipSpace <* char ']'

------------------------------------------------------------------------------
-- NOTA BENE                                                                --
--                                                                          --
-- no whitespace before or after an argtuple!                               --
                                                                            --
argTuple :: Parser a -> Parser [a]                                          --
argTuple p =                                                                --
  id <$ char '(' <* skipSpace                                               --
  <*> sep (punc ',') p                                                      --
  <* skipSpace <* char ')'                                                  --
                                                                            --
--                                                                          --
------------------------------------------------------------------------------

variable :: Parser RawVariable
variable = do
  start <- identifier
  next  <- choice [ Just <$ char '.' <*> identifier
                  , pure Nothing ]
  pure $ case next of
    Nothing  -> (Nothing, start)
    Just end -> (Just start, end)

spaceMaybeComma :: Parser ()
spaceMaybeComma =
  () <$ skipSpace <* (() <$ char ',' <* skipSpace <|> pure ())

clause :: Parser RawClause
clause = (,) <$> sep spaceMaybeComma pcomputation <* skipSpace <* arrow <* skipSpace
             <*> term
  <|> (,) [] <$> term

pcomputation :: Parser PComputation
pcomputation
  =   PValue <$> pvalue
  <|> id <$ char '{' <* skipSpace <*>
      (    PThunk <$> identifier
       <|> PRequest <$> ((,) <$> atom <*> argTuple pvalue) <* skipSpace
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
  , (\ (kw, ps, lit) -> PString kw ps lit) <$> spliceOf pvalue
  , PAtom <$> atom
  , pvar
  , PWild <$ char '_'
  , uncurry (flip $ foldr PCell) <$> listOf pvalue PNil
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
getMeATerm = getMeA (spaceTerm <* endOfInput)
