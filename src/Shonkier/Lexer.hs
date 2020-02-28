module Shonkier.Lexer where

import Data.Char
import Data.Text as T

data Literal
  = Text Text Text
  | Num !Integer
  deriving Show

data Punctuation
  = LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | LAngle
  | RAngle
  | Comma
  | Pipe
  deriving Show

data Token' a
  = Id String
  | Atom a
  | Lit Literal
  | Punc Punctuation
  | Arrow
  deriving Show

type Token = Token' String
type Lexer = Text -> [Token]

lexing :: Lexer
lexing start = maybe [] (uncurry dispatch) (uncons txt)  where

  txt = stripStart start

  dispatch :: Char -> Lexer
  dispatch c t = case c of
    '(' -> Punc LParen   : lexing t
    ')' -> Punc RParen   : lexing t
    '[' -> Punc LBracket : lexing t
    ']' -> Punc RBracket : lexing t
    '{' -> Punc LBrace   : lexing t
    '}' -> Punc LBrace   : lexing t
    '<' -> Punc LAngle   : lexing t
    '>' -> Punc RAngle   : lexing t
    ',' -> Punc Comma    : lexing t
    '|' -> Punc Pipe     : lexing t
    '"' -> stringlitBy "" t
    '`' -> atom t
    '-' -> arrow txt
    _   | isDigit c -> numlit txt
        | isAlpha c -> identifier txt
        | otherwise -> stringlit txt

  -- Precondition: first character is '-'
  arrow :: Lexer
  arrow t = maybe (stringlit t) ((Arrow :) . lexing)
          $ stripPrefix "->" t

  -- Precondition: first character is alpha
  identifier :: Lexer
  identifier t =
    let (id, rest) = T.span isAlphaNum t in
    -- This may actually be the start of a stringlit!
    case uncons rest of
      Just ('"', t) -> stringlitBy id t
      _             -> Id (T.unpack id) : lexing rest

  atom :: Lexer
  atom t = case uncons t of
    Just (c, t) | isAlpha c ->
      let (cs, rest) = T.span isAlphaNum t in
      Atom (c : T.unpack cs) : lexing rest
    _ -> error "Atom expected"

  stringlit :: Lexer
  stringlit t =
    let (pref, rest) = T.span ('"' /=) t in
    stringlitBy pref (T.tail rest)

  stringlitBy :: Text -> Lexer
  stringlitBy pref t =
    let key         = cons '"' pref
        (str, rest) = breakOn key t
        n           = T.length key
    in if T.null rest
    then error "Unfinished string literal"
    else Lit (Text pref str) : lexing (T.drop n rest)

  -- Precondition: first character is a digit
  numlit :: Lexer
  numlit t =
    let (ds, rest) = T.span isDigit t in
    -- This may actually be the start of a stringlit!
    case uncons rest of
      Just ('"', t) -> stringlitBy ds t
      _ -> Lit (Num $ read $ T.unpack ds) : lexing rest
