{-# LANGUAGE ScopedTypeVariables #-}

module Shonkier.Primitives where

import Control.Applicative

import Data.Map (singleton)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Shonkier.Syntax
import Shonkier.Value
import {-# SOURCE #-} Shonkier.Semantics

---------------------------------------------------------------------------
-- /!\ Do not forget to also implement the primitive in the js interpreter!
---------------------------------------------------------------------------

primEnv :: GlobalEnv
primEnv = foldMap toVal primitives where
  toVal (str, _) = singleton str $ singleton "." $ VPrim str []

type PRIMITIVE = [Computation] -> Shonkier Computation

prim :: Primitive -> PRIMITIVE
prim nm vs = case lookup nm primitives of
  Nothing -> handle ("NoPrim",[VPrim nm []]) []
  Just f  -> f vs

primitives :: [(Primitive, PRIMITIVE)]
primitives =
  [ ("primStringConcat", primStringConcat)
  , ("primNumAdd"      , primNumAdd)
  , ("primNumMinus"    , primNumMinus)
  , ("primNumMult"     , primNumMult)
  ]

---------------------------------------------------------------------------
-- NUM

primNumBin :: String -> (Rational -> Rational -> Rational)
           -> PRIMITIVE
primNumBin nm op = \case
  [CNum m, CNum n]   -> use (VNum (op m n))
  [Value m, Value n] -> handle ("Invalid_" ++ nm ++ "_ArgType", [m, n]) []
  [_,_]              -> handle ("Invalid_" ++ nm ++ "Add_ArgRequest",[]) []
  _                  -> handle ("Invalid_" ++ nm ++ "_Arity", []) []

primNumAdd, primNumMinus, primNumMult :: PRIMITIVE
primNumAdd   = primNumBin "primNumAdd" (+)
primNumMinus = primNumBin "primNumMinus" (-)
primNumMult  = primNumBin "primNumMult" (*)

---------------------------------------------------------------------------
-- STRING

primStringConcat :: PRIMITIVE
primStringConcat cs = go cs Nothing [] where

  go :: [Computation] -> Maybe Keyword -> [Text] -> Shonkier Computation
  go cs mk ts = case cs of
    []                 -> let txt = T.concat $ reverse ts
                          in use (VString (fromMaybe "" mk) txt)
    (CString k t : cs) -> go cs (mk <|> pure k) (t:ts)
    (CCell a b   : cs) -> go (Value a : Value b : cs) mk ts
    (CAtom {}    : cs) -> go cs mk ts
    (Value v     : cs) -> handle ("Invalid_StringConcat_ArgType", [v]) []
    _                  -> handle ("Invalid_StringConcat_ArgRequest",[]) []