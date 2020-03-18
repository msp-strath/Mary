{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, LambdaCase #-}

module Shonkier.ShonkierJS where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.List
import Data.Char
import Data.Ratio
import Data.Map (foldMapWithKey)

import Shonkier.Syntax
import Shonkier.Value

class JSAtom a where
  jsAtom :: a -> Text

instance JSAtom String where
  jsAtom a = T.concat ["\"",pack a,"\""]

class JS t where
  js :: t -> [Text]

instance JS Text where
  js t = [t]

instance JS a => JS [a] where
  js as = ["["] ++ Data.List.intercalate [","] (fmap js as) ++ ["]"]

instance JS ScopedVariable where
  js = \case
    LocalVar v     -> [jsAtom v]
    GlobalVar fp v -> ["GVar(", jsAtom fp, ",", jsAtom v, ")"]
    _              -> ["null"]

instance (JS v, JSAtom a) => JS (Clause' v a) where
  js (qs, t) = ["Clause("] ++ js qs ++ [","] ++ js t ++ [")"]

instance (JS v, JSAtom a) => JS (Term' v a) where
  js (Atom a)    = ["Atom(", jsAtom a, ")"]
  js (Lit l)     = ["Lit("] ++ js l ++ [")"]
  js (Var x)     = js x
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
    ["Request(", jsAtom a] ++ [","] ++ js ps ++ [",\"", maybe "_" pack k, "\")"]
  js (PThunk x) = ["\"",pack x,"\""]

instance JSAtom a => JS (PValue' a) where
  js (PAtom a)    = ["Atom(", jsAtom a, ")"]
  js (PLit l)     = ["Lit("] ++ js l ++ [")"]
  js (PBind x)    = ["\"",pack x,"\""]
  js (PAs x p)    = ["PAs("] ++ ["\"",pack x,"\""] ++ [","] ++ js p ++ [")"]
  js (PCell s t)  = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js PWild        = ["PWild"]

jsGlobalEnv :: GlobalEnv -> [Text]
jsGlobalEnv gl =
  "var globalEnv = {};\n" :
  ((`foldMapWithKey` gl) $ \ x loc ->
    ((T.concat [ "globalEnv[", jsAtom x, "] = {};\n"]) :) $
    flip foldMapWithKey loc $ \ fp -> \case
    VFun [] _ hs cs -> pure $ T.concat $
      ["globalEnv[", jsAtom x, "][", jsAtom fp ,"] = VFun(null,{},"]
      ++ js (fmap (fmap jsAtom) hs) ++ [","]
      ++ js cs
      ++ [");\n"]
    VPrim g hs ->  pure $ T.concat $
      ["globalEnv[", jsAtom x, "][", jsAtom fp, "] = VPrim(", jsAtom g , ","]
      ++ js (fmap (fmap jsAtom) hs)
      ++ [");\n"]
    _ -> [])

jsMain :: FilePath -> Text
jsMain fp = T.concat
  [ "console.log("
  , "render("
  , "shonkier(globalEnv,App("
  , "GVar(\"", T.pack fp, "\",\"main\"), []))"
  , "));"
  ]
