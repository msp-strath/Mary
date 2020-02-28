module Shonkier.Lexer where

import Data.Char
import Data.Text as T

data Literal
  = Text !Int Text
  | Num !Integer

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

data Token' a
  = Id String
  | Atom a
  | Lit Literal
  | Punc Punctuation
  | Arrow

type Token = Token' String
type Lexer = Text -> [Token]

lexing :: Lexer
lexing txt = maybe [] (uncurry dispatch) $ uncons (stripStart txt) where

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
    '"' -> stringlit t
    '`' -> atom t
    '-' -> arrow txt
    _   | isAlpha c -> identifier txt
        | isDigit c -> numlit txt
        | otherwise -> undefined

  arrow :: Lexer
  arrow t = maybe (identifier t) ((Arrow :) . lexing)
          $ stripPrefix "->" t

  identifier :: Lexer
  identifier t =
    let (id, rest) = T.span isAlphaNum t in
    Id (T.unpack id) : lexing rest

  atom :: Lexer
  atom t = case uncons t of
    Just (c, t) | isAlpha c ->
      let (cs, rest) = T.span isAlphaNum t in
      Atom (c : T.unpack cs) : lexing rest
    _ -> undefined

  stringlit :: Lexer
  stringlit t =
    let (pref, t')  = T.span ('"' ==) t
        key         = cons '"' pref
        (str, rest) = breakOn key t'
        n           = T.length key
    in if T.null rest
    then undefined
    else Lit (Text n str) : lexing (T.drop n rest)

  numlit :: Lexer
  numlit t =
    let (ds, rest) = T.span isDigit t in
    Lit (Num $ read $ T.unpack ds) : lexing rest
