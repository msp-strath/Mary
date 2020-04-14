{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, LambdaCase #-}

module Shonkier.ShonkierJS where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List
import Data.Ratio
import Data.Map (foldMapWithKey)
import Data.Maybe (fromMaybe)

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

instance JS Scoping where
  js = \case
    LocalVar           -> ["LocalVar()"]
    GlobalVar b fp     -> ["GlobalVar("] ++ js b ++ [",", jsAtom fp, ")"]
    AmbiguousVar _     -> ["AmbiguousVar()"]
    OutOfScope         -> ["OutOfScope()"]
    InvalidNamespace _ -> ["InvalidNamespace()"]

instance JS ScopedVariable where
  js (sco :.: x) = ["Var("] ++ js sco ++ [",", jsAtom x, ")"]

instance (JS v) => JS (Clause' String v) where
  js (qs, r) = ["Clause("] ++ js qs ++ [","] ++ js (rhs2Term r) ++ [")"] where

instance (JS v) => JS (Term' String v) where
  js (Atom a)        = ["Atom(", jsAtom a, ")"]
  js (Lit l)         = ["Lit("] ++ js l ++ [")"]
  js (String _ ts u) = ["Stringy("] ++ jsStringy ts u ++ [")"] where
  js (Var x)         = js x
  js Nil             = ["Nil()"]
  js (Cell s t)      = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js (App f as)      = ["App("] ++ js f ++ [","] ++ js as ++ [")"]
  js (Semi l r)      = ["Semi("] ++ js l ++ [","] ++ js r ++ [")"]
  js (Prio l r)      = ["Prio("] ++ js l ++ [","] ++ js r ++ [")"]
  js (Fun hs cs)     = ["Fun("] ++ js (fmap (fmap jsAtom) hs) ++ [","]
                    ++ js cs
                    ++ [")"]
  js (Match p t)     = ["Match("] ++ js p ++ [","] ++ js t ++ [")"]
  js (Mask a t)      = ["Mask(", jsAtom a, ","] ++ js t ++ [")"]

instance JS Literal where
  js (Num r) = ["LitNum(",pack (show (numerator r)),",",pack (show (denominator r)),")"]
  js (Boolean b) = js b

instance JS Bool where
  js True  = ["true"]
  js False = ["false"]

instance JS (PComputation' String) where
  js (PValue p) = ["Value("] ++ js p ++ [")"]
  js (PRequest (a, ps) k) =
    ["Request(", jsAtom a] ++ [","] ++ js ps ++ [",\"", maybe "_" pack k, "\")"]
  js (PThunk x) = ["\"",pack x,"\""]

instance JS (PValue' String) where
  js (PAtom a)        = ["Atom(", jsAtom a, ")"]
  js (PLit l)         = ["Lit("] ++ js l ++ [")"]
  js (PString _ ts u) = ["Stringy("] ++ jsStringy ts u ++ [")"] where
  js (PBind x)        = ["\"",pack x,"\""]
  js (PAs x p)        = ["PAs("] ++ ["\"",pack x,"\""] ++ [","] ++ js p ++ [")"]
  js PNil             = ["Nil()"]
  js (PCell s t)      = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js PWild            = ["PWild"]

jsText :: Text -> [Text]
jsText t
  | T.any (`elem` ['"', '\\']) t = [T.pack (show t)]
  | otherwise = ["\"", t, "\""]

jsStringy :: JS x => [(Text, x)] -> Text -> [Text]
jsStringy []             u = jsText u
jsStringy ((t, x) : txs) u =
  ["Strunk("] ++ jsText t ++ [","] ++ js x ++ [","] ++ jsStringy txs u ++ [")"]

jsGlobalEnv :: GlobalEnv -> [Text]
jsGlobalEnv gl =
  "var globalEnv = {};\n" :
  ((`foldMapWithKey` gl) $ \ x loc ->
    ((T.concat [ "globalEnv[", jsAtom x, "] = {};\n"]) :) $
    flip foldMapWithKey loc $ \ fp -> \case
    VFun CnNil _ hs cs -> pure $ T.concat $
      ["globalEnv[", jsAtom x, "][", jsAtom fp ,"] = VFun(null,{},"]
      ++ js (fmap (fmap jsAtom) hs) ++ [","]
      ++ js cs
      ++ [");\n"]
    VPrim g hs ->  pure $ T.concat $
      ["globalEnv[", jsAtom x, "][", jsAtom fp, "] = VPrim(", jsAtom g , ","]
      ++ js (fmap (fmap jsAtom) hs)
      ++ [");\n"]
    _ -> [])

jsRun :: Term -> [Text]
jsRun t = [ "console.log(render(shonkier(globalEnv,"] ++ js t ++ [")));"]
