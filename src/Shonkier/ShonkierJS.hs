{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, LambdaCase #-}

module Shonkier.ShonkierJS where

import Data.Char
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.List
import Data.Ratio
import Data.Map (Map, foldMapWithKey)
import qualified Data.Set as Set

import Shonkier.Syntax
import Shonkier.Value

class JSAtom a where
  jsAtom :: a -> Text

instance JSAtom String where
  jsAtom a = T.concat ["\"",pack a,"\""]

instance JSAtom Atom where
  jsAtom = jsAtom . getAtom

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

instance (JS ns, JSAtom a, JS v, Atomy a, Vary v) => JS (Clause' ns a v) where
  js (qs :-> rs) = ["Clause("] ++ js qs ++ [","] ++ js (rhs2Term rs) ++ [")"] where

instance (JS ns, JSAtom a, JS v, Atomy a, Vary v) => JS (Term' ns a v) where
  js (Atom a)        = ["Atom(", jsAtom a, ")"]
  js (Lit l)         = ["Lit("] ++ js l ++ [")"]
  js (String _ ts u) = ["Stringy("] ++ jsStringy ts u ++ [")"] where
  js (Var x)         = js x
  js (Namespace ns)  = ["Namespace("] ++ js ns ++ [")"]
  js Nil             = ["Nil()"]
  js Blank           = ["undefined"]
  js (Cell s t)      = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js (App f as)      = ["App("] ++ js f ++ [","] ++ js as ++ [")"]
  js (Semi l r)      = ["Semi("] ++ js l ++ [","] ++ js r ++ [")"]
  js (Prio l r)      = ["Prio("] ++ js l ++ [","] ++ js r ++ [")"]
  js (Fun hs cs)     = ["Fun("] ++ js (fmap (fmap jsAtom) hs) ++ [","]
                    ++ js (braceFun cs)
                    ++ [")"]
  js (Match p t)     = ["Match("] ++ js p ++ [","] ++ js t ++ [")"]
  js (Mask a t)      = ["Mask(", jsAtom a, ","] ++ js t ++ [")"]

instance JS Namespace where
  js (MkNamespace ns fps) = [jsAtom ns, ","] ++ js (fmap jsAtom $ Set.toList fps)

instance JS Literal where
  js (Num r) = [ "LitNum(",pack (show (numerator r))
               ,",", pack (show (denominator r)),")"
               ]
  js (Boolean b) = js b

instance JS Bool where
  js True  = ["true"]
  js False = ["false"]

instance JSAtom a => JS (PComputation' a) where
  js (PValue p) = ["Value("] ++ js p ++ [")"]
  js (PRequest (a, ps) k) = ["Request(", jsAtom a]
                                ++ [","] ++ js ps
                                ++ [","] ++ ["\"", maybe "_" pack k, "\")"]
  js (PThunk x) = ["\"",pack x,"\""]

instance JSAtom a => JS (PValue' a) where
  js (PAtom a)        = ["Atom(", jsAtom a, ")"]
  js (PLit l)         = ["Lit("] ++ js l ++ [")"]
  js (PString _ ts u) = ["Stringy("] ++ jsStringy ts u ++ [")"] where
  js (PBind x)        = ["\"",pack x,"\""]
  js (PAs x p)        = ["PAs("] ++ ["\"",pack x,"\""]
                        ++ [","] ++ js p ++ [")"]
  js PNil             = ["Nil()"]
  js (PCell s t)      = ["Cell("] ++ js s ++ [","] ++ js t ++ [")"]
  js PWild            = ["PWild"]

jsText :: Text -> [Text]
jsText t
  | T.any (\ z -> z `elem` ['"', '\\'] || isControl z) t = [T.pack (show t)]
  | otherwise = ["\"", t, "\""]

jsStringy :: JS x => [(Text, x)] -> Text -> [Text]
jsStringy []             u = jsText u
jsStringy ((t, x) : txs) u =
  ["Strunk("] ++ jsText t ++ [","] ++ js x ++ [","] ++ jsStringy txs u ++ [")"]

jsInputs :: Map Text Text -> [Text]
jsInputs inp =
  "var inputs = {};\n" :
  ((`foldMapWithKey` inp) $ \ field val ->
    pure $ T.concat $ ["inputs["] ++
                      jsText field ++
                      ["] = "] ++
                      jsText val ++
                      [";\n"])

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
jsRun t = [ "console.log(render(shonkier(globalEnv,inputs,"] ++ js t ++ [")));"]
