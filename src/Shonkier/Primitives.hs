{-# LANGUAGE ScopedTypeVariables #-}

module Shonkier.Primitives where

import Control.Applicative

import Data.Attoparsec.Text (parseOnly)

import Data.Map (singleton)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Lisp
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
  Nothing -> complain "NoPrim" [VPrim nm []]
  Just f  -> f vs

primitives :: [(Primitive, PRIMITIVE)]
primitives =
  [ ("primStringConcat"   , primStringConcat)
  , ("primPrefixNot"      , primPrefixNot)
  , ("primInfixAnd"       , primInfixAnd)
  , ("primInfixOr"        , primInfixOr)
  , ("primInfixEquals"    , primInfixEquals)
  , ("primInfixUnequal"   , primInfixUnequal)
  , ("primInfixLessEq"    , primInfixLessEq)
  , ("primInfixGreaterEq" , primInfixGreaterEq)
  , ("primInfixLess"      , primInfixLess)
  , ("primInfixGreater"   , primInfixGreater)
  , ("primInfixPlus"      , primInfixPlus)
  , ("primInfixMinus"     , primInfixMinus)
  , ("primInfixTimes"     , primInfixTimes)
  , ("primInfixOver"      , primInfixOver)
  , ("primNumToString"    , primNumToString)
  , ("primStringToNum"    , primStringToNum)
  , ("primPickle"         , primPickle)
  , ("primUnpickle"       , primUnpickle)
  ]


---------------------------------------------------------------------------
-- EQUALS OR NOT

primInfixEquals, primInfixUnequal :: PRIMITIVE
primInfixEquals [Value x, Value y] = case valueEqHuh x y of
  Nothing -> complain "higherOrderEqTest" []
  Just b  -> use (VBoolean b)
primInfixEquals _ = complain "Invalid_primInfixEquals_Arity" []

primInfixUnequal [Value x, Value y] = case valueEqHuh x y of
  Nothing -> complain "higherOrderEqTest" []
  Just b  -> use (VBoolean (not b))
primInfixUnequal _ = complain "Invalid_primInfixUnequal_Arity" []

---------------------------------------------------------------------------
-- NUM

primNumBin :: String -> (Rational -> Rational -> Rational)
           -> PRIMITIVE
primNumBin nm op = \case
  [CNum m, CNum n]   -> use (VNum (op m n))
  [Value m, Value n] -> complaining "ArgType"    [m, n]
  [_,_]              -> complaining "ArgRequest" []
  _                  -> complaining "Arity"      []
  where complaining str = complain (MkAtom $ "Invalid_" ++ nm ++ "_" ++ str)

primInfixPlus, primInfixMinus, primInfixTimes :: PRIMITIVE
primInfixPlus  = primNumBin "primInfixPlus"  (+)
primInfixMinus = primNumBin "primInfixMinus" (-)
primInfixTimes = primNumBin "primInfixTimes" (*)

primInfixOver :: PRIMITIVE
primInfixOver [_, CNum 0] = complain "divByZero" []
primInfixOver as = primNumBin "primInfixOver" (/) as

primNumToString :: PRIMITIVE
primNumToString = \case
  [CNum m]  -> use (VString "" (ppRational Utopia m))
  [Value m] -> complaining "ArgType"    [m]
  [_]       -> complaining "ArgRequest" []
  _         -> complaining "Arity"      []
  where complaining = complain . MkAtom . ("Invalid_primNumToString_" ++)

primNumBoo :: String -> (Rational -> Rational -> Bool)
           -> PRIMITIVE
primNumBoo nm op = \case
  [CNum m, CNum n]   -> use (VBoolean (op m n))
  [Value m, Value n] -> complaining "ArgType"    [m, n]
  [_,_]              -> complaining "ArgRequest" []
  _                  -> complaining "Arity"      []
  where complaining str = complain (MkAtom $ "Invalid_" ++ nm ++ "_" ++ str)

primInfixLessEq, primInfixGreaterEq, primInfixLess, primInfixGreater :: PRIMITIVE
primInfixLessEq    = primNumBoo "primInfixLessEq"    (<=)
primInfixGreaterEq = primNumBoo "primInfixGreaterEq" (>=)
primInfixLess      = primNumBoo "primInfixLess"      (<)
primInfixGreater   = primNumBoo "primInfixGreater"   (>)


---------------------------------------------------------------------------
-- BOOLEAN

primPrefixNot :: PRIMITIVE
primPrefixNot = \case
  [CBoolean b]   -> use (VBoolean (not b))
  [Value n]      -> complaining "ArgType"    [n]
  [_]            -> complaining "ArgRequest" []
  _              -> complaining "Arity"      []
  where complaining = complain . MkAtom . ("Invalid_primPrefixNot_" ++)

primBooBin :: String -> (Bool -> Bool -> Bool)
           -> PRIMITIVE
primBooBin nm op = \case
  [CBoolean m, CBoolean n]   -> use (VBoolean (op m n))
  [Value m, Value n] -> complaining "ArgType"    [m, n]
  [_,_]              -> complaining "ArgRequest" []
  _                  -> complaining "Arity"      []
  where complaining str = complain (MkAtom $ "Invalid_" ++ nm ++ "_" ++ str)

primInfixAnd, primInfixOr :: PRIMITIVE
primInfixAnd = primBooBin "primInfixAnd" (&&)
primInfixOr  = primBooBin "primInfixOr"  (||)


---------------------------------------------------------------------------
-- STRING

primStringToNum :: PRIMITIVE
primStringToNum = \case
  [CString _ s]  -> case parseOnly numlit s of
    Left err -> complain "Invalid_primStringToNum_ParseError" [VString "" (T.pack err)]
    Right (Num n) -> use (VNum n)
    Right (Boolean b) -> complain "Invalid_primStringToNum_ValueType" [VBoolean b]
  [Value m] -> complain "Invalid_primStringToNum_ArgType" [m]
  [_]       -> complain "Invalid_primStringToNum_ArgRequest" []
  _         -> complain "Invalid_primStringToNum_Arity" []

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
    (Value v     : cs) -> complain "Invalid_StringConcat_ArgType" [v]
    _                  -> complain "Invalid_StringConcat_ArgRequest" []


---------------------------------------------------------------------------
-- PICKLING

primPickle :: PRIMITIVE
primPickle = \case
  [Value v] -> use (VString "" (lispText (toLISP v)))
  _ -> complain "PickleAValue" []

primUnpickle :: PRIMITIVE
primUnpickle = \case
  [Value (VString _ t)] -> case textLisp t >>= fromLISP of
    Just v -> use v
    _ -> abort
  _ -> complain "UnpickleAString" []
  