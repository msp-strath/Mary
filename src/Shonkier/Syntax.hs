{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wincomplete-patterns #-}

module Shonkier.Syntax where

import Data.Bwd

data Term' a
  = Atom a
  | Var String
  | Cell (Term' a) (Term' a)
  | App (Term' a) [Term' a]

type Term = Term' String

data Val' a
  = VAtom a
  | VCell (Val' a) (Val' a)

type Val = Val' String

-- Evaluation contexts

-- The argument of type () indicates the
-- cursor position
data Frame' a
  = CellL (Env' a) (Term' a)
  | CellR (Val' a) (Env' a)
  | AppL (Env' a) [Term' a]
  | AppR a (Bwd (Val' a)) (Env' a) [Term' a]

type Frame = Frame' String

type Context' a = Bwd (Frame' a)
type Context = Context' String

-- Evaluation functions

type Env' a = String -> Maybe (Val' a)
type Env = Env' String

type Request' a = (a, [Val' a])
type Request = Request' String

data Computation' a
  = Value (Val' a)
  | Request (Request' a) [Frame' a]
  -- ^ Invoking an effect & none of the
  -- frames present know how to interpret it

type Computation = Computation' String

eval :: Context -> (Env, Term) -> Computation
eval ctx (rho, t) = case t of
  Var x    -> case rho x of
    Just v  -> use ctx v
    Nothing -> handle ctx ("OutOfScope", []) []
  -- move left; start evaluating left to right
  Atom a   -> use ctx (VAtom a)
  Cell a b -> eval (ctx :< CellL rho b) (rho, a)
  App f as -> eval (ctx :< AppL rho as) (rho, f)

use :: Context -> Val -> Computation
use Nil         v = Value v
use (ctx :< fr) v = case fr of
  -- move right or upwards
  CellL rho b -> eval (ctx :< CellR v rho) (rho, b)
  CellR u rho -> use ctx (VCell u v)
  -- we better be using a function
  AppL rho as -> case v of
    VAtom f -> app ctx f Nil rho as
    _       -> handle ctx ("NoFun",[v]) []
  AppR f vz rho as -> app ctx f (vz :< v) rho as

app :: Context -> String -> Bwd Val -> Env -> [Term]
    -> Computation
app ctx f vz rho as = case as of
  []       -> handle ctx (f, vz <>> []) []
  (a : as) -> eval (ctx :< AppR f vz rho as) (rho, a)

handle :: Context -> Request -> [Frame]
       -> Computation
handle ctx r frs = case ctx of
  Nil         -> Request r frs
  (ctx :< fr) -> handle ctx r (fr : frs)
