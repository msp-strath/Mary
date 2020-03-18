{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module Shonkier.Pretty where

import Shonkier.Syntax
import Shonkier.Semantics

import Control.Arrow ((***))

import Data.Char
import Data.Foldable
import Data.Ratio
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

ppAtom :: String -> Doc ann
ppAtom = \case
  [] -> "[]"
  a  -> squote <> pretty a

{-
Lisp conventions for ppList:
[xs]           |-> [xs]
[1|[2|[3|xs]]] |-> [1 2 3|xs]
[1|[2|[3|[]]]] |-> [1 2 3]
-}

ppList :: Pretty a => ([a], Maybe a) -> Doc ann
ppList (as, m) = encloseSep lbracket closing space $ pretty <$> as
  where
  closing = maybe mempty ((pipe <>) . pretty) m <> rbracket

data ListView a la = ItsNil | ItsCons a la | ItsNot

listView :: (la -> ListView a la) -> la -> ([a], Maybe la)
listView coalg seed = case coalg seed of
  ItsNil       -> ([], Nothing)
  ItsCons x xs -> (x :) *** id $ listView coalg xs
  ItsNot       -> ([], Just seed)

termCoalg :: Term -> ListView Term Term
termCoalg = \case
  Atom ""  -> ItsNil
  Cell a b -> ItsCons a b
  _        -> ItsNot

valueCoalg :: Value -> ListView Value Value
valueCoalg = \case
  VAtom ""  -> ItsNil
  VCell a b -> ItsCons a b
  _         -> ItsNot

pvalueCoalg :: PValue -> ListView PValue PValue
pvalueCoalg = \case
  PAtom ""  -> ItsNil
  PCell a b -> ItsCons a b
  _         -> ItsNot

ppApp :: Pretty a => Doc ann -> [a] -> Doc ann
ppApp f ts = f <> tupled (pretty <$> ts)

ppFun :: (Pretty a, Pretty b) => [[a]] -> [b] -> Doc ann
ppFun hs cls = encloseSep lbrace rbrace semi $ pretty <$> cls

ppClause :: Clause -> Doc ann
ppClause (ps, t) = tupled (pretty <$> ps) <+> "->" <+> pretty t

ppStringLit :: String -> T.Text -> Doc ann
ppStringLit k str = enclose (key <> dquote) (dquote <> key) (pretty str) where

  tk = T.pack ('"' : k)

  key   = pretty $ case maximum ((-2):occ) of
    (-2) -> k
    n    -> k ++ show (n + 1)
  occ   = [ d | tl <- T.tails str
              , suff <- toList $ T.stripPrefix tk tl
              , let d = mread . T.unpack $ T.takeWhile isDigit suff
              ]
  mread = \case
    [] -> (-1)
    ds -> read ds

instance Pretty Rational where
  pretty p =
    let n = numerator p; d = denominator p in
    if | d == 1    -> pretty n
       | d == 2    -> pretty (n `div` 2) <> ".5"
       | d == 4    -> pretty (n `div` 4)
                      <> if n `mod` 4 == 1 then ".25" else ".75"
       | otherwise -> pretty n <+> "/" <+> pretty d

instance Pretty Literal where
  pretty = \case
    String k str -> ppStringLit k str
    Num r        -> pretty r

instance Pretty ScopedVariable where
  pretty = \case
    LocalVar x       -> pretty x
    GlobalVar _ x    -> pretty x
    AmbiguousVar _ x -> pretty x
    OutOfScope x     -> pretty x

instance Pretty Term where
  pretty t = case listView termCoalg t of
    ([], Just _) -> case t of
      Atom a     -> ppAtom a
      Lit l      -> pretty l
      Var v      -> pretty v
      Cell a b   -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
      App f ts   -> ppApp (pretty f) ts
      Fun hs cls -> ppFun hs cls
    it -> ppList it

instance Pretty PValue where
  pretty p = case listView pvalueCoalg p of
    ([], Just _) -> case p of
      PAtom a   -> ppAtom a
      PLit l    -> pretty l
      PBind v   -> pretty v
      PAs v p   -> pretty v <> "@" <> pretty p
      PWild     -> "_"
      PCell a b -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
    it -> ppList it

instance Pretty PComputation where
  pretty = \case
    PValue p           -> pretty p
    PRequest (a, vs) v -> braces $ hsep [ppApp (ppAtom a) vs, "->", pretty v]
    PThunk v           -> braces $ pretty v

instance Pretty Value where
  pretty v = case listView valueCoalg v of
    ([], Just _) -> case v of
      VAtom a          -> ppAtom a
      VLit l           -> pretty l
      VPrim f _        -> pretty f
      VCell a b        -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
      VFun _ _ hs cls  -> ppFun hs cls
      VThunk c         -> braces $ pretty c
    it -> ppList it

instance Pretty Computation where
  pretty = \case
    Value v             -> pretty v
    Request (a, vs) frs -> ppApp (ppAtom a) vs
