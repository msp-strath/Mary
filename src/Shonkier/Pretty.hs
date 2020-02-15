{-# LANGUAGE FlexibleInstances #-}

module Shonkier.Pretty where

import Shonkier.Syntax
import Shonkier.Semantics

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

instance Pretty Term where
  pretty = \case
    Atom a     -> ppAtom a
    Var v      -> pretty v
    Cell a b   -> ppCell a b
    App f ts   -> ppApp (pretty f) ts
    Fun hs cls -> ppFun hs cls

instance Pretty PValue where
  pretty = \case
    PAtom a   -> ppAtom a
    PBind v   -> pretty v
    PCell a b -> ppCell a b

instance Pretty PComputation where
  pretty = \case
    PValue p           -> pretty p
    PRequest (a, vs) v -> angles $ hsep [ppApp (ppAtom a) vs, "->", pretty v]
    PThunk v           -> angles $ pretty v

instance Pretty Value where
  pretty = \case
    VAtom a            -> ppAtom a
    VCell a b          -> ppCell a b
    VFun fr rho hs cls -> ppFun hs cls
    VThunk c           -> angles $ pretty c

instance Pretty Computation where
  pretty = \case
    Value v             -> pretty v
    Request (a, vs) frs -> ppApp (ppAtom a) vs
