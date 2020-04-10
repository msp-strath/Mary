{-# LANGUAGE ScopedTypeVariables #-}

module Shonkier.Primitives where

import Control.Applicative

import Data.Attoparsec.Text (parseOnly)

import Data.Map (singleton)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Shonkier.Syntax
import Shonkier.Value
import {-# SOURCE #-} Shonkier.Semantics
import Shonkier.Parser (numlit)
import Shonkier.Pretty (ppRational)

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
  , ("primNumToString" , primNumToString)
  , ("primStringToNum" , primStringToNum)
  ]

---------------------------------------------------------------------------
-- NUM

primNumBin :: String -> (Rational -> Rational -> Rational)
           -> PRIMITIVE
primNumBin nm op = \case
  [CNum m, CNum n]   -> use (VNum (op m n))
  [Value m, Value n] -> handle ("Invalid_" ++ nm ++ "_ArgType", [m, n]) []
  [_,_]              -> handle ("Invalid_" ++ nm ++ "_ArgRequest",[]) []
  _                  -> handle ("Invalid_" ++ nm ++ "_Arity", []) []

primNumAdd, primNumMinus, primNumMult :: PRIMITIVE
primNumAdd   = primNumBin "primNumAdd" (+)
primNumMinus = primNumBin "primNumMinus" (-)
primNumMult  = primNumBin "primNumMult" (*)

primNumToString :: PRIMITIVE
primNumToString = \case
  [CNum m]  -> use (VString "" (ppRational m))
  [Value m] -> handle ("Invalid_primNumToString_ArgType", [m]) []
  [_]       -> handle ("Invalid_primNumToString_ArgRequest",[]) []
  _         -> handle ("Invalid_primNumToString_Arity", []) []

---------------------------------------------------------------------------
-- STRING

primStringToNum :: PRIMITIVE
primStringToNum = \case
  [CString _ s]  -> case parseOnly numlit s of
    Left err -> handle ("Invalid_primStringToNum_ParseError", [VString "" (T.pack err)]) []
    Right (Num n) -> use (VNum n)
    Right (Boolean b) -> handle ("Invalid_primStringToNum_ValueType", [VBoolean b]) []
  [Value m] -> handle ("Invalid_primStringToNum_ArgType", [m]) []
  [_]       -> handle ("Invalid_primStringToNum_ArgRequest",[]) []
  _         -> handle ("Invalid_primStringToNum_Arity", []) []

primStringConcat :: PRIMITIVE
primStringConcat cs = go cs Nothing [] where

  go :: [Computation] -> Maybe Keyword -> [Text] -> Shonkier Computation
  go cs mk ts = case cs of
    []                 -> let txt = T.concat $ reverse ts
                          in use (VString (fromMaybe "" mk) txt)
    (CString k t : cs) -> go cs (mk <|> pure k) (t:ts)
    (CCell a b   : cs) -> go (Value a : Value b : cs) mk ts
    (CAtom {}    : cs) -> go cs mk ts
    (CNil        : cs) -> go cs mk ts
    (Value v     : cs) -> handle ("Invalid_StringConcat_ArgType", [v]) []
    _                  -> handle ("Invalid_StringConcat_ArgRequest",[]) []
