module Shonkier.Syntax where

import Data.Text
import Utils.List

type Keyword   = String
type Primitive = String
type Namespace = String

type Variable     = String
type RawVariable  = (Maybe Namespace, Variable)

data ScopedVariable
  = LocalVar Variable
  | GlobalVar FilePath Variable
  | AmbiguousVar [FilePath] Variable
  | InvalidNamespace Namespace Variable
  | OutOfScope Variable
  deriving (Show)

data Literal
  = String Keyword Text
  | Num Rational
  deriving (Show)

data Term' a v
  = Atom a
  | Lit Literal
  | Var v
  | Cell (Term' a v) (Term' a v)
  | App (Term' a v) [Term' a v]
  | Fun [[a]] [Clause' a v]
  deriving (Show, Functor)

type RawTerm = Term' String RawVariable
type Term    = Term' String ScopedVariable

pattern TString k str = Lit (String k str)
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
  | PBind Variable
  | PWild
  | PAs Variable (PValue' a)
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
    Atom ""  -> ItsNil
    Cell a b -> ItsCons a b
    _        -> ItsNot

instance HasListView PValue PValue where
  coalgebra = \case
    PAtom ""  -> ItsNil
    PCell a b -> ItsCons a b
    _         -> ItsNot
