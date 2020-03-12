{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, LambdaCase #-}

module Shonkier.ShonkierJS where

import Data.Text
import Data.List
import Data.Char
import Data.Ratio
import Data.Map (foldMapWithKey)
import Shonkier.Syntax
import Shonkier.Semantics

class JSAtom a where
  jsAtom :: a -> Text

instance JSAtom String where
  jsAtom a = Data.Text.concat ["\"",pack a,"\""]

class JS t where
  js :: t -> [Text]

instance JS Text where
  js t = [t]

instance JS a => JS [a] where
  js as = ["["] ++ Data.List.intercalate [","] (fmap js as) ++ ["]"]

instance JSAtom a => JS (Clause' a) where
  js (qs, t) = ["Clause("] ++ js qs ++ [","] ++ js t ++ [")"]

instance JSAtom a => JS (Term' a) where
  js (Atom a)    = ["Atom(", jsAtom a, ")"]
  js (Lit l)     = ["Lit("] ++ js l ++ [")"]
  js (Var x)     = ["\"",pack x,"\""]
  js (Cell s t)  = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js (App f as)  = ["App("] ++ js f ++ [","] ++ js as ++ [")"]
  js (Fun hs cs) = ["Fun("] ++ js (fmap (fmap jsAtom) hs) ++ [","]
                ++ js cs
                ++ [")"]
  
instance JS Literal where
  js (String _ txt) = ["\""] ++ (unpack txt >>= ch) ++ ["\""] where
    ch c | c < ' ' = ["\\", pack [chr (64 + ord c)]]
    ch '"' = ["\\\""]
    ch '\\' = ["\\\\"]
    ch c = [pack [c]]
  js (Num r) = ["LitNum(",pack (show (numerator r)),",",pack (show (denominator r)),")"]

instance JSAtom a => JS (PComputation' a) where
  js (PValue p) = ["Value("] ++ js p ++ [")"]
  js (PRequest (a, ps) k) =
    ["Request(", jsAtom a] ++ [","] ++ js ps ++ [",\"", pack k, "\")"]
  js (PThunk x) = ["\"",pack x,"\""]

instance JSAtom a => JS (PValue' a) where
  js (PAtom a)    = ["Atom(", jsAtom a, ")"]
  js (PLit l)     = ["Lit("] ++ js l ++ [")"]
  js (PBind x)    = ["\"",pack x,"\""]
  js (PCell s t)  = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js PWild        = ["PWild"]

jsGlobalEnv :: Program -> Text
jsGlobalEnv ls = Data.Text.concat $
  "var globalEnv = {}; " :
  ((`foldMapWithKey` mkGlobalEnv ls) $ \ f -> \case
    VFun [] _ hs cs ->
      ["globalEnv[", jsAtom f, "] = VFun(null,{},"]
      ++ js (fmap (fmap jsAtom) hs) ++ [","]
      ++ js cs
      ++ ["); "]
    _ -> [])