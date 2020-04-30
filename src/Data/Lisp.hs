module Data.Lisp (LISP(..), lispText, lispMunch) where

import qualified Data.Text as T
import Control.Applicative
import Data.Char
import Data.Ratio

data LISP
  = ATOM String
  | CONS LISP LISP
  | NIL
  | RAT Rational
  | STR Text

type Text = T.Text

num :: Integer -> Text
num = T.pack . show

blat :: LISP -> [Text] -> [Text]
blat (ATOM x)   = (T.pack x :)
blat (CONS a d) = ("[" :) . blat a . bcdr d . ("]" :) where
  bcdr NIL         = id
  bcdr (CONS a d)  = (" " :) . blat a . bcdr d
  bcdr x           = ("|" :) . blat x
blat NIL           = ("[]" :)
blat (RAT r)       = (num (numerator r) :) .
  case denominator r of
    1 -> id
    d -> ("/" :) . (num d :)
blat (STR s) = quo . (s :) . quo
  where
  quo = ("'" :) . (foo :). ("'" :)
  foo = case [n | n <- T.splitOn "'" s, T.all isDigit n] of
    [] -> ""
    ns -> num (maximum (negate 1 : map (read . ('0' :) . T.unpack) ns) + 1) 

lispText :: LISP -> Text
lispText x = T.concat (blat x [])

instance Show LISP where show = T.unpack . lispText

lispMunch :: Text -> Either Text (LISP, Text)
lispMunch t = case T.uncons t of
    Just (c, u)
      | isSpace c -> lispMunch u
      | isAlpha c -> let (x, v) = T.span isAlphaNum u in
          Right (ATOM (c : T.unpack x), v)
      | isDigit c -> let (n, v) = numFrom c u in
          case T.uncons v of
            Just ('/', w) -> case T.uncons w of
              Just (c, w) | isDigit c -> let (d, x) = numFrom c w in
                Right (RAT (n % d), x)
              _ -> Right (RAT (n % 1), v)
                   -- 1/foo is 1 followed by garbage
            _ -> Right (RAT (n % 1), v)
      | c == '-'  -> case lispMunch u of
        Right (RAT r, u)  -> Right (RAT (negate r), u)
        _                 -> Left t
      | c == '['  -> listMunch u
      | c == '\'' -> case T.span (/= '\'') u of
        (n, u) | T.all isDigit n -> case T.uncons u of
          Just (_, u) ->  -- the span ensures that _ is '\''
            let quo = T.concat ["'", n, "'"]
                (s, v) = T.breakOn quo u
            in  case T.stripPrefix quo v of
                  Just v -> Right (STR s, v)
                  _      -> Left t
          _ -> Left t
        _ -> Left t
    _ -> Left t
  where
    numFrom :: Char {-digit-} -> Text -> (Integer, Text)
    numFrom d x = (read (d : T.unpack ds), y)
      where
      (ds, y) = T.span isDigit x
    listMunch u = case T.uncons u of
      Just (c, v)
        | isSpace c -> listMunch v
        | c == ']' -> Right (NIL, v)
        | c == '|' -> do
          (d, v) <- lispMunch v
          let (_, w) = T.span isSpace v
          case T.uncons w of
            Just (']', w) -> Right (d, w)
            _ -> Left t
        | otherwise -> do
          (a, u) <- lispMunch u
          (d, u) <- listMunch u
          return (CONS a d, u)
      _ -> Left u