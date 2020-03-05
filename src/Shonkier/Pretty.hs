{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

module Shonkier.Pretty where

import Shonkier.Syntax
import Shonkier.Semantics

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

ppCell :: Pretty a => a -> a -> Doc ann
ppCell a b = encloseSep lbracket rbracket pipe $ pretty <$> [a, b]

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

instance Pretty Term where
  pretty = \case
    Atom a     -> ppAtom a
    Lit l      -> pretty l
    Var v      -> pretty v
    Cell a b   -> ppCell a b
    App f ts   -> ppApp (pretty f) ts
    Fun hs cls -> ppFun hs cls

instance Pretty PValue where
  pretty = \case
    PAtom a   -> ppAtom a
    PLit l    -> pretty l
    PBind v   -> pretty v
    PWild     -> "_"
    PCell a b -> ppCell a b

instance Pretty PComputation where
  pretty = \case
    PValue p           -> pretty p
    PRequest (a, vs) v -> angles $ hsep [ppApp (ppAtom a) vs, "->", pretty v]
    PThunk v           -> angles $ pretty v

instance Pretty Value where
  pretty = \case
    VAtom a            -> ppAtom a
    VLit l             -> pretty l
    VPrim f _          -> pretty f
    VCell a b          -> ppCell a b
    VFun fr rho hs cls -> ppFun hs cls
    VThunk c           -> angles $ pretty c

instance Pretty Computation where
  pretty = \case
    Value v             -> pretty v
    Request (a, vs) frs -> ppApp (ppAtom a) vs
