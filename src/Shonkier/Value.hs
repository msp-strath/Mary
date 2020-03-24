module Shonkier.Value where

import Control.Monad.State
import Control.Monad.Reader

import Data.Map (Map)
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2

import Data.Bwd
import Shonkier.Syntax
import Utils.List

---------------------------------------------------------------------------
-- ENVIRONMENTS
---------------------------------------------------------------------------

type GlobalEnv' a = Map Variable (Map FilePath (Value' a))
type GlobalEnv = GlobalEnv' String

type LocalEnv' a = Map Variable (Value' a)
type LocalEnv = LocalEnv' String

merge :: LocalEnv' a -> LocalEnv' a -> LocalEnv' a
merge = flip (<>)

---------------------------------------------------------------------------
-- VALUES
---------------------------------------------------------------------------

data Value' a
  = VAtom a
  | VLit Literal
  | VPrim Primitive [[a]]
  | VCell (Value' a) (Value' a)
  | VFun [Frame' a] (LocalEnv' a) [[a]] [Clause' ScopedVariable a]
  -- ^ Env is the one the function was created in
  --   Frames ??
  | VThunk (Computation' a)
  deriving (Show)

type Value = Value' String

pattern VNum n        = VLit (Num n)
pattern CNum n        = Value (VNum n)
pattern VString k str = VLit (String k str)
pattern CString k str = Value (VString k str)
pattern VNil          = VAtom ""
pattern CCell a b     = Value (VCell a b)
pattern CAtom a       = Value (VAtom a)

---------------------------------------------------------------------------
-- COMPUTATIONS
---------------------------------------------------------------------------

type Request' a = (a, [Value' a])
type Request = Request' String

data Computation' a
  = Value (Value' a)
  | Request (Request' a) [Frame' a]
  -- ^ Invoking an effect & none of the
  -- frames present know how to interpret it
  deriving (Show)

type Computation = Computation' String

---------------------------------------------------------------------------
-- EVALUATION CONTEXTS
---------------------------------------------------------------------------

data Funy' a
  = FAtom a
  | FPrim Primitive
  | FFun [Frame' a] (LocalEnv' a) [Clause' ScopedVariable a]
  deriving (Show)
type Funy = Funy' String

-- The argument of type (LocalEnv' a) indicates the
-- cursor position
data Frame' a
  = CellL (LocalEnv' a) (Term' ScopedVariable a)
  | CellR (Value' a) (LocalEnv' a)
  | AppL (LocalEnv' a) [Term' ScopedVariable a]
  | AppR (Funy' a)
         (Bwd (Computation' a))
         -- ^ already evaluated arguments (some requests we are
         --   willing to handle may still need to be dealt with)
         ([a], LocalEnv' a)
         -- ^ focus: [a] = requests we are willing to handle
         [([a], Term' ScopedVariable a)]
         -- ^ each arg comes with requests we are willing to handle
  | SemiL (LocalEnv' a) (Term' ScopedVariable a)
  | LetL (PValue' a) (LocalEnv' a) (Term' ScopedVariable a)
  deriving (Show)

type Frame = Frame' String

type Context' a = Bwd (Frame' a)
type Context = Context' String

---------------------------------------------------------------------------
-- EVALUATION MONAD
---------------------------------------------------------------------------

newtype Shonkier a = Shonkier
  { getShonkier :: StateT Context (Reader GlobalEnv) a }
  deriving ( Functor, Applicative, Monad
           , MonadState Context, MonadReader GlobalEnv
           )

---------------------------------------------------------------------------
-- INSTANCES
---------------------------------------------------------------------------

instance HasListView Value Value where
  coalgebra = \case
    VAtom ""  -> ItsNil
    VCell a b -> ItsCons a b
    _         -> ItsNot
