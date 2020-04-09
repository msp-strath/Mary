module Shonkier.Value where

import Control.Monad.State
import Control.Monad.Reader

import Data.Map (Map)
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text (Text)

import Data.Bwd
import Shonkier.Syntax
import Utils.List

---------------------------------------------------------------------------
-- ENVIRONMENTS
---------------------------------------------------------------------------

type GlobalEnv' a v = Map Variable (Map FilePath (Value' a v))
type GlobalEnv = GlobalEnv' String ScopedVariable

type LocalEnv' a v = Map Variable (Value' a v)
type LocalEnv = LocalEnv' String ScopedVariable

merge :: LocalEnv' a v -> LocalEnv' a v -> LocalEnv' a v
merge = flip (<>)

---------------------------------------------------------------------------
-- VALUES
---------------------------------------------------------------------------

data Value' a v
  = VAtom a
  | VLit Literal
  | VString Keyword Text
  | VPrim Primitive [[a]]
  | VCell (Value' a v) (Value' a v)
  | VFun [Frame' a v] (LocalEnv' a v) [[a]] [Clause' a v]
  -- ^ Env is the one the function was created in
  --   Frames ??
  | VThunk (Computation' a v)
  deriving (Show, Functor)

type Value = Value' String ScopedVariable

pattern VNum n        = VLit (Num n)
pattern CNum n        = Value (VNum n)
pattern CString k str = Value (VString k str)
pattern VNil          = VAtom ""
pattern CCell a b     = Value (VCell a b)
pattern CAtom a       = Value (VAtom a)

---------------------------------------------------------------------------
-- COMPUTATIONS
---------------------------------------------------------------------------

type Request' a v = (a, [Value' a v])
type Request = Request' String ScopedVariable

data Computation' a v
  = Value (Value' a v)
  | Request (Request' a v) [Frame' a v]
  -- ^ Invoking an effect & none of the
  -- frames present know how to interpret it
  deriving (Show, Functor)

type Computation = Computation' String ScopedVariable

---------------------------------------------------------------------------
-- EVALUATION CONTEXTS
---------------------------------------------------------------------------

data Funy' a v
  = FAtom a
  | FPrim Primitive
  | FFun [Frame' a v] (LocalEnv' a v) [Clause' a v]
  deriving (Show, Functor)
type Funy = Funy' String ScopedVariable

-- The argument of type (LocalEnv' a) indicates the
-- cursor position
data Frame' a v
  = CellL (LocalEnv' a v) (Term' a v)
  | CellR (Value' a v) (LocalEnv' a v)
  | AppL (LocalEnv' a v) [Term' a v]
  | AppR (Funy' a v)
         (Bwd (Computation' a v))
         -- ^ already evaluated arguments (some requests we are
         --   willing to handle may still need to be dealt with)
         ([a], LocalEnv' a v)
         -- ^ focus: [a] = requests we are willing to handle
         [([a], Term' a v)]
         -- ^ each arg comes with requests we are willing to handle
  | SemiL (LocalEnv' a v) (Term' a v)
  | StringLR (Value' a v) (LocalEnv' a v) [(Text, Term' a v)] Text
  deriving (Show, Functor)

type Frame = Frame' String ScopedVariable

type Context' a v = Bwd (Frame' a v)
type Context = Context' String ScopedVariable

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

---------------------------------------------------------------------------
-- FROMVALUE
---------------------------------------------------------------------------

class FromValue t where
  fromValue :: Value -> t

instance FromValue t => FromValue [t] where
  fromValue (VCell t ts) = fromValue t : fromValue ts
  fromValue _ = []

instance (FromValue a, FromValue b) => FromValue (a, b) where
  fromValue (VCell a b) = (fromValue a, fromValue b)
  fromValue _ = (fromValue VNil, fromValue VNil)

instance (FromValue a, FromValue b, FromValue c) => FromValue (a, b, c) where
  fromValue (VCell a (VCell b c)) = (fromValue a, fromValue b, fromValue c)
  fromValue _ = (fromValue VNil, fromValue VNil, fromValue VNil)
