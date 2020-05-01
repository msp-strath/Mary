module Data.Lisp (LISP(..), LISPY(..), (-:), spil, lispText, lispMunch) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Data.Ratio

import Data.Bwd

data LISP
  = ATOM String
  | CONS LISP LISP
  | NIL
  | RAT Rational
  | STR Text
  | BOO Bool

class LISPY t where
  toLISP    :: t -> LISP
  fromLISP  :: LISP -> Maybe t

(-:) :: String -> [LISP] -> LISP
c -: ts = CONS (ATOM c) (foldr CONS NIL ts)

spil :: LISP -> Maybe (String, [LISP])
spil (CONS (ATOM c) d) = (c,) <$> go d where
  go NIL        = pure []
  go (CONS a d) = (a :) <$> go d
  go _          = Nothing
spil _ = Nothing

instance LISPY LISP where
  toLISP   = id
  fromLISP = Just

instance LISPY Text where
  toLISP = STR
  fromLISP (STR t) = Just t
  fromLISP _       = Nothing

instance LISPY Rational where
  toLISP = RAT
  fromLISP (RAT r) = Just r
  fromLISP _       = Nothing

instance LISPY Bool where
  toLISP = BOO
  fromLISP (BOO b) = Just b
  fromLISP _       = Nothing

instance (LISPY s, LISPY t) => LISPY (Either s t) where
  toLISP (Left s)  = "Left" -: [toLISP s]
  toLISP (Right t) = "Right" -: [toLISP t]
  fromLISP t = spil t >>= \case
    ("Left",  [s]) -> Left <$> fromLISP s
    ("Right", [t]) -> Right <$> fromLISP t
    _ -> Nothing

instance (LISPY s, LISPY t) => LISPY (s, t) where
  toLISP (s, t) = CONS (toLISP s) (toLISP t)
  fromLISP (CONS s t) = (,) <$> fromLISP s <*> fromLISP t
  fromLISP _          = Nothing

instance (LISPY s, LISPY t, LISPY u) => LISPY (s, t, u) where
  toLISP (s, t, u) = CONS (toLISP s) (CONS (toLISP t) (toLISP u))
  fromLISP (CONS s (CONS t u)) = (,,) <$> fromLISP s <*> fromLISP t <*> fromLISP u
  fromLISP _          = Nothing


instance LISPY x => LISPY [x] where
  toLISP = foldr (CONS . toLISP) NIL
  fromLISP NIL         = pure []
  fromLISP (CONS x xs) = (:) <$> fromLISP x <*> fromLISP xs
  fromLISP _           = Nothing

instance LISPY x => LISPY (Maybe x) where
  toLISP = maybe NIL ((`CONS` NIL) . toLISP)
  fromLISP NIL          = pure Nothing
  fromLISP (CONS x NIL) = Just <$> fromLISP x
  fromLISP _            = Nothing

instance LISPY x => LISPY (Bwd x) where
  -- keeping the near end near means reversing or ugliness
  -- choosing reversing for the now
  toLISP B0        = NIL
  toLISP (xz :< x) = CONS (toLISP x) (toLISP xz)
  fromLISP NIL         = pure B0
  fromLISP (CONS x xz) = (:<) <$> fromLISP xz <*> fromLISP x
  fromLISP _           = Nothing

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
blat (BOO False) = ("#0" :)
blat (BOO True)  = ("#1" :)

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
      | c == '#' -> case T.uncons u of
        Just ('0', v) -> Right (BOO False, v)
        Just ('1', v) -> Right (BOO True,  v)
        _ -> Left t
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
