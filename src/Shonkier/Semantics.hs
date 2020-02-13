{-# LANGUAGE LambdaCase           #-}
{-# OPTIONS -Wincomplete-patterns #-}

module Shonkier.Semantics where

import Control.Monad
import Data.Map (Map, singleton, (!?))

import Data.Bwd
import Shonkier.Syntax

data Val' a
  = VAtom a
  | VCell (Val' a) (Val' a)
  | VFun [Frame' a] (Env' a) [Clause' a]
  -- ^ Env is the one the function was created in
  --   Frames ??
  deriving (Show)

type Val = Val' String

-- environments

type Env' a = Map String (Val' a)
type Env = Env' String

merge :: Env' a -> Env' a -> Env' a
merge = flip (<>)

match :: Pattern -> Val -> Maybe Env
match (PAtom a)   (VAtom b)   = mempty <$ guard (a == b)
match (PBind x)   v           = pure (singleton x v)
match (PCell p q) (VCell v w) = merge <$> match p v <*> match q w
match _ _ = Nothing

mayZipWith :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
mayZipWith f []       []       = pure []
mayZipWith f (a : as) (b : bs) =
  (:) <$> f a b <*> mayZipWith f as bs
mayZipWith _ _ _ = Nothing


matches :: [Pattern] -> [Val] -> Maybe Env
matches ps vs = foldl merge mempty <$> mayZipWith match ps vs


-- Evaluation contexts

data Funy' a
  = FAtom a
  | FFun [Frame' a] (Env' a) [Clause' a]
  deriving (Show)
type Funy = Funy' String

-- The argument of type (Env' a) indicates the
-- cursor position
data Frame' a
  = CellL (Env' a) (Term' a)
  | CellR (Val' a) (Env' a)
  | AppL (Env' a) [Term' a]
  | AppR (Funy' a) (Bwd (Val' a)) (Env' a) [Term' a]
  deriving (Show)

type Frame = Frame' String

type Context' a = Bwd (Frame' a)
type Context = Context' String

-- Evaluation functions

type Request' a = (a, [Val' a])
type Request = Request' String

data Computation' a
  = Value (Val' a)
  | Request (Request' a) [Frame' a]
  -- ^ Invoking an effect & none of the
  -- frames present know how to interpret it
  deriving (Show)

type Computation = Computation' String

eval :: Context -> (Env, Term) -> Computation
eval ctx (rho, t) = case t of
  Var x    -> case rho !? x of
    Just v  -> use ctx v
    Nothing -> handle ctx ("OutOfScope", []) []
  -- move left; start evaluating left to right
  Atom a   -> use ctx (VAtom a)
  Cell a b -> eval (ctx :< CellL rho b) (rho, a)
  App f as -> eval (ctx :< AppL rho as) (rho, f)
  Fun cs   -> use ctx (VFun [] rho cs)


use :: Context -> Val -> Computation
use Nil         v = Value v
use (ctx :< fr) v = case fr of
  -- move right or upwards
  CellL rho b -> eval (ctx :< CellR v rho) (rho, b)
  CellR u rho -> use ctx (VCell u v)
  -- we better be making a request or using a function
  AppL rho as -> case v of
    VAtom f         -> app ctx (FAtom f)        Nil rho as
    VFun frs sig cs -> app ctx (FFun frs sig cs) Nil rho as
    _               -> handle ctx ("NoFun",[v]) []
  AppR f vz rho as -> app ctx f (vz :< v) rho as

app :: Context -> Funy -> Bwd Val -> Env -> [Term]
    -> Computation
app ctx f vz rho as = case as of
  []       -> case f of
    FAtom a         -> handle ctx (a, vz <>> []) []
    FFun frs sig cs -> call (ctx <>< frs) sig cs (vz <>> [])
  (a : as) -> eval (ctx :< AppR f vz rho as) (rho, a)

handle :: Context -> Request -> [Frame]
       -> Computation
handle ctx r frs = case ctx of
  Nil         -> Request r frs
  (ctx :< fr) -> handle ctx r (fr : frs)

call :: Context -> Env -> [Clause] -> [Val]
     -> Computation
call ctx rho []               vs =
  handle ctx ("IncompletePattern", vs) []
call ctx rho ((ps, rhs) : cs) vs = case matches ps vs of
  Nothing  -> call ctx rho cs vs
  Just sig -> eval ctx (merge rho sig, rhs)

