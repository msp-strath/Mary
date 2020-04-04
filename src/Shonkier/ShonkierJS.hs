{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, LambdaCase #-}

module Shonkier.ShonkierJS where

import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List
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
    LocalVar v         -> [jsAtom v]
    GlobalVar _ fp v   -> ["GVar(", jsAtom fp, ",", jsAtom v, ")"]  -- FIXME!
    -- error cases
    AmbiguousVar _ x     -> exception "AmbiguousVar" x
    OutOfScope x         -> exception "OutOfScope" x
    InvalidNamespace _ x -> exception "InvalidNamespace" x

    where
      exception :: String -> Variable -> [Text]
      exception at x = js (App (Atom at) [vVar x] :: Term)
      vVar x = String "" [] (T.pack x)

instance (JS v, JSAtom a) => JS (Clause' a v) where
  js (qs, t) = ["Clause("] ++ js qs ++ [","] ++ js t ++ [")"]

instance (JS v, JSAtom a) => JS (Term' a v) where
  js (Atom a)        = ["Atom(", jsAtom a, ")"]
  js (Lit l)         = ["Lit("] ++ js l ++ [")"]
  js (String _ ts u) = ["Stringy("] ++ jsStringy ts u ++ [")"] where
  js (Var x)         = js x
  js (Cell s t)      = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js (App f as)      = ["App("] ++ js f ++ [","] ++ js as ++ [")"]
  js (Semi l r)      = ["Semi("] ++ js l ++ [","] ++ js r ++ [")"]
  js (Fun hs cs)     = ["Fun("] ++ js (fmap (fmap jsAtom) hs) ++ [","]
                    ++ js cs
                    ++ [")"]

instance JS Literal where
  js (Num r) = ["LitNum(",pack (show (numerator r)),",",pack (show (denominator r)),")"]

instance JSAtom a => JS (PComputation' a) where
  js (PValue p) = ["Value("] ++ js p ++ [")"]
  js (PRequest (a, ps) k) =
    ["Request(", jsAtom a] ++ [","] ++ js ps ++ [",\"", maybe "_" pack k, "\")"]
  js (PThunk x) = ["\"",pack x,"\""]

instance JSAtom a => JS (PValue' a) where
  js (PAtom a)        = ["Atom(", jsAtom a, ")"]
  js (PLit l)         = ["Lit("] ++ js l ++ [")"]
  js (PString _ ts u) = ["Stringy("] ++ jsStringy ts u ++ [")"] where
  js (PBind x)        = ["\"",pack x,"\""]
  js (PAs x p)        = ["PAs("] ++ ["\"",pack x,"\""] ++ [","] ++ js p ++ [")"]
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

jsRun :: Term -> [Text]
jsRun t = [ "console.log(render(shonkier(globalEnv,"] ++ js t ++ [")));"]
