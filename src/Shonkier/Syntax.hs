module Shonkier.Syntax where

type Variable = String
type Keyword  = String

data Literal
  = String Keyword String
  deriving (Show)

data Term' a
  = Atom a
  | Lit Literal
  | Var Variable
  | Cell (Term' a) (Term' a)
  | App (Term' a) [Term' a]
  | Fun [[a]] [Clause' a]
  deriving (Show)

type Term = Term' String

type Clause' a = ([PComputation' a], Term' a)
type Clause = Clause' String

data PValue' a
  = PAtom a
  | PLit Literal
  | PBind Variable
  | PCell (PValue' a) (PValue' a)
  deriving (Show)
type PValue = PValue' String

data PComputation' a
  = PValue (PValue' a)
  | PRequest (a, [PValue' a]) Variable
    -- ^ var: resumption
  | PThunk Variable
    -- ^ grab any of the computations we are willing
    --   to handle (values are considered trivial comps).
  deriving (Show)
type PComputation = PComputation' String
