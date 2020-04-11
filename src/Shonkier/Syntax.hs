module Shonkier.Syntax where

import Data.Text
import Utils.List

type Keyword   = String
type Primitive = String
type Namespace = String

type Variable     = String
type RawVariable  = (Maybe Namespace, Variable)

data ScopedVariable = (:.:)
  { scoping :: Scoping
  , nameOf  :: Variable
  } deriving Show

data Scoping
  = LocalVar
  | GlobalVar Bool{-from longname?-} FilePath
  | AmbiguousVar [FilePath]
  | InvalidNamespace Namespace
  | OutOfScope
  deriving (Show)

data Literal
  = Num Rational
  | Boolean Bool
  deriving (Show, Eq)

data Term' a v
  = Atom a
  | Nil
  | Lit Literal
  | String Keyword [(Text, Term' a v)] Text
  | Var v
  | Cell (Term' a v) (Term' a v)
  | App (Term' a v) [Term' a v]
  | Semi (Term' a v) (Term' a v)
  | Prio (Term' a v) (Term' a v)
  | Fun [[a]] [Clause' a v]
  | Match (PValue' a) (Term' a v)
  deriving (Show, Functor)

type RawTerm = Term' String RawVariable
type Term    = Term' String ScopedVariable

pattern TNum n        = Lit (Num n)

type Import = (FilePath, Maybe Namespace)
type Program' a v = [(String, Either [[String]] (Clause' a v))]
type Module'  a v = ([Import], Program' a v)

type RawProgram = Program' String RawVariable
type Program    = Program' String ScopedVariable
type RawModule  = Module' String RawVariable
type Module     = Module' String ScopedVariable

type Clause' a v = ([PComputation' a], Term' a v)
type RawClause = Clause' String RawVariable
type Clause    = Clause' String ScopedVariable

data PValue' a
  = PAtom a
  | PLit Literal
  | PString Keyword [(Text, PValue' a)] Text
  | PBind Variable
  | PWild
  | PAs Variable (PValue' a)
  | PNil
  | PCell (PValue' a) (PValue' a)
  deriving (Show)
type PValue = PValue' String

data PComputation' a
  = PValue (PValue' a)
  | PRequest (a, [PValue' a]) (Maybe Variable) -- we may throw the binder away
    -- ^ var: resumption
  | PThunk Variable
    -- ^ grab any of the computations we are willing
    --   to handle (values are considered trivial comps).
  deriving (Show)
type PComputation = PComputation' String

---------------------------------------------------------------------------
-- INSTANCES
---------------------------------------------------------------------------

instance HasListView (Term' String v) (Term' String v) where
  coalgebra = \case
    Nil      -> ItsNil
    Cell a b -> ItsCons a b
    _        -> ItsNot

instance HasListView PValue PValue where
  coalgebra = \case
    PNil      -> ItsNil
    PCell a b -> ItsCons a b
    _         -> ItsNot

---------------------------------------------------------------------------
-- TORAWTERM
---------------------------------------------------------------------------

class ToRawTerm t where
  toRawTerm :: t -> RawTerm
