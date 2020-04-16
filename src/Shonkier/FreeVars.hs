{-# LANGUAGE GADTs #-}

module Shonkier.FreeVars where

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Shonkier.Syntax
import Shonkier.Value

class FreeVars t where
  freeVars :: t -> Set Variable

  default freeVars :: (t ~ f a, Foldable f, FreeVars a) => t -> Set Variable
  freeVars = foldMap freeVars

instance FreeVars t => FreeVars [t]
instance FreeVars t => FreeVars (Maybe t)
instance FreeVars t => FreeVars (Map k t)

instance FreeVars RawVariable where
  freeVars (mns, x) = maybe Set.empty Set.singleton (x <$ mns)

instance FreeVars ScopedVariable where
  freeVars (scope :.: x) = case scope of
    LocalVar           -> Set.singleton x
    GlobalVar{}        -> Set.empty
    AmbiguousVar{}     -> Set.empty
    InvalidNamespace{} -> Set.empty
    OutOfScope{}       -> Set.empty

instance FreeVars v => FreeVars (Term' a v) where
  freeVars = \case
    Atom{}         -> Set.empty
    Lit{}          -> Set.empty
    Nil{}          -> Set.empty
    String _ tes _ -> foldMap (freeVars . snd) tes
    Var v          -> freeVars v
    Cell a b       -> freeVars [a, b]
    App f ts       -> freeVars (f:ts)
    -- To be exact we would need to subtract the variables bound in `f`
    -- if it is a `Match`. At the moment we generate an over-approximation.
    Semi a b       -> freeVars [a, b]
    Fun _ cls      -> freeVars cls
    Match p e      -> freeVars e

instance FreeVars (PValue' a) where
  freeVars = \case
    PAtom{}         -> Set.empty
    PLit{}          -> Set.empty
    PWild{}         -> Set.empty
    PNil{}          -> Set.empty
    PBind v         -> Set.singleton v
    PString _ tps _ -> foldMap (freeVars . snd) tps
    PAs v p         -> Set.insert v (freeVars p)
    PCell a b       -> freeVars [a, b]

instance FreeVars (PComputation' a) where
  freeVars = \case
    PValue v            -> freeVars v
    PRequest (_, ps) mk -> Set.union (freeVars ps) (maybe Set.empty Set.singleton mk)
    PThunk k            -> Set.singleton k

instance FreeVars v => FreeVars (Clause' a v) where
  freeVars (ps, t) = Set.difference (freeVars t) (freeVars ps)

instance FreeVars v => FreeVars (Rhs' a v) where
  freeVars (gd :?> tm) = Set.union (freeVars gd) (freeVars tm)

instance FreeVars v => FreeVars (Value' a v) where
  freeVars = \case
    VAtom{}          -> Set.empty
    VLit{}           -> Set.empty
    VNil{}           -> Set.empty
    VString{}        -> Set.empty
    VPrim{}          -> Set.empty
    VCell a b        -> freeVars [a, b]
    VFun _ rho _ cls -> Set.union (freeVars rho) (freeVars cls)
    VThunk c         -> freeVars c

instance FreeVars v => FreeVars (Computation' a v) where
  freeVars = \case
    Value v           -> freeVars v
    Request (_, vs) _ -> freeVars vs
