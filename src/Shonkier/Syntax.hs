module Shonkier.Syntax where

import Data.Text

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

data Term' v a
  = Atom a
  | Lit Literal
  | Var v
  | Cell (Term' v a) (Term' v a)
  | App (Term' v a) [Term' v a]
  | Fun [[a]] [Clause' v a]
  deriving (Show)

type RawTerm = Term' RawVariable String
type Term    = Term' ScopedVariable String

pattern TString k str = Lit (String k str)
pattern TNum n        = Lit (Num n)

type Import = (FilePath, Maybe Namespace)
type Program' v a = [(String, Either [[String]] (Clause' v a))]
type Module'  v a = ([Import], Program' v a)

type RawProgram = Program' RawVariable String
type Program    = Program' ScopedVariable String
type RawModule  = Module' RawVariable String
type Module     = Module' ScopedVariable String

type Clause' v a = ([PComputation' a], Term' v a)
type RawClause = Clause' RawVariable String
type Clause    = Clause' ScopedVariable String

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
