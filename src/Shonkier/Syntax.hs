module Shonkier.Syntax where

data Term' a
  = Atom a
  | Var String
  | Cell (Term' a) (Term' a)
  | App (Term' a) [Term' a]
  | Fun [Clause' a]
  deriving (Show)

type Term = Term' String

type Clause' a = ([Pattern' a], Term' a)
type Clause = Clause' String

data Pattern' a
  = PAtom a
  | PBind String
  | PCell (Pattern' a) (Pattern' a)
  deriving (Show)
type Pattern = Pattern' String
