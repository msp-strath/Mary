{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Shonkier.Syntax where

import Control.Monad.State

import Data.Text
import Utils.List

type Keyword   = String
type Primitive = String
type Namespace = String
type Operator  = String

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
  | Blank  -- for brace sections
  | Cell (Term' a v) (Term' a v)
  | App (Term' a v) [Term' a v]
  | Semi (Term' a v) (Term' a v)
  | Prio (Term' a v) (Term' a v)
  | Fun [[a]] [Clause' a v]
  | Match (PValue' a) (Term' a v)
  | Mask a (Term' a v)
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

type Clause' a v = ([PComputation' a], [Rhs' a v])
type RawClause = Clause' String RawVariable
type Clause    = Clause' String ScopedVariable
data Rhs' a v = Maybe (Term' a v) :?> Term' a v
  deriving (Show, Functor)
type Rhs = Rhs' String ScopedVariable
type RawRhs = Rhs' String RawVariable

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


---------------------------------------------------------------------------
-- FIXITY
---------------------------------------------------------------------------

data Tightness
  -- funky control
  = PriT   -- e1 ; e2 ?> e3    ->    (e1 ; e2) ?> e3
  | SemT   -- p := e1 ; e2     ->    (p := e1) ; e2
  | PaMa   -- p := 'a ^ e      ->    p := ('a ^ e)
  | MasT   -- 'a ^ e1 \/ e2    ->    'a ^ (e1 \/ e2)

  -- boolean things
  | Disj   -- a \/ b /\ c      ->    a \/ (b /\ c)
  | Conj   -- a /\ ! b         ->    a /\ (! b)
  | Nega   -- ! x == y         ->    ! (x == y)
  | Comp   -- x == y + 1       ->    x == (y + 1)
  
  -- arithmetic
  | Addy
  | Mult

  -- application
  | Appl   -- 2 * f(x)         ->    2 * (f(x))
  deriving (Show, Ord, Eq, Enum, Bounded)

data Associativity
  = LAsso | NAsso | RAsso
  deriving (Show, Eq)

data OpFax = OpFax
  { tight :: Tightness
  , assoc :: Associativity
  , spell :: String
  } deriving (Show, Eq)

data Dir = LeftOf | RightOf deriving (Show, Eq)
data WhereAmI
  = Utopia
  | Dir :^: OpFax
  deriving (Show, Eq)

needParens :: OpFax -> WhereAmI -> Bool
needParens _ Utopia = False
needParens x (d :^: y) = case compare (tight x) (tight y) of
  LT -> True
  EQ -> case (d, assoc y) of
    (LeftOf, LAsso)  -> False
    (RightOf, RAsso) -> False
    _                -> True
  GT -> False

opChars :: String
opChars = "-=!\\/<>+*/"

prioFax, semiFax, pamaFax, maskFax, overFax, applFax :: OpFax
prioFax = OpFax {tight = PriT, assoc = RAsso, spell = "Prio"}
semiFax = OpFax {tight = SemT, assoc = RAsso, spell = "Semi"}
pamaFax = OpFax {tight = PaMa, assoc = RAsso, spell = "PaMa"}
maskFax = OpFax {tight = MasT, assoc = RAsso, spell = "Mask"}
overFax = OpFax {tight = Mult, assoc = LAsso, spell = "Over"}
applFax = OpFax {tight = Appl, assoc = LAsso, spell = "Appl"}

-- morally...
-- ?>   Prio RAsso
-- ;    Semi RAsso
-- :=   PaMa RAsso
-- ^    Mask RAsso

infixOpFax :: [(String, OpFax)]
infixOpFax =
  [ ("\\/", OpFax {tight = Disj, assoc = LAsso, spell = "Or"})
  , ("/\\", OpFax {tight = Conj, assoc = LAsso, spell = "And"})
  , ("==",  OpFax {tight = Comp, assoc = LAsso, spell = "Equals"})
  , ("!=",  OpFax {tight = Comp, assoc = LAsso, spell = "Unequal"})
  , ("<=",  OpFax {tight = Comp, assoc = LAsso, spell = "LessEq"})
  , (">=",  OpFax {tight = Comp, assoc = LAsso, spell = "GreaterEq"})
  , ("<",   OpFax {tight = Comp, assoc = LAsso, spell = "Less"})
  , (">",   OpFax {tight = Comp, assoc = LAsso, spell = "Greater"})
  , ("+",   OpFax {tight = Addy, assoc = LAsso, spell = "Plus"})
  , ("-",   OpFax {tight = Addy, assoc = LAsso, spell = "Minus"})
  , ("*",   OpFax {tight = Mult, assoc = LAsso, spell = "Times"})
  , ("/",   overFax)
  ]

prefixOpFax :: [(String, OpFax)]
prefixOpFax =
  [ ("!", OpFax {tight = Nega, assoc = RAsso, spell = "Not"})
  ]


---------------------------------------------------------------------------
-- BRACE SECTIONS
---------------------------------------------------------------------------

class Vary v where
  varOf :: Variable -> v

instance Vary Variable where
  varOf = id
instance Vary RawVariable where
  varOf = (Nothing,)
instance Vary ScopedVariable where
  varOf = (LocalVar :.:)  -- you better know what you're doing

braceFun :: Vary v => [Clause' a v] -> [Clause' a v]
braceFun cs = cs' where
  uv :: forall w. Vary w => Int -> w
  uv i = varOf $ "_" ++ show i
  (cs', n) = runState (traverse mangle cs) 0
  ups = [PValue (PBind (uv i)) | i <- [0..(n - 1)]]
  mangle (ps, rs) = (ups ++ ps,) <$> traverse wrangle rs
  wrangle (g :?> t) = (:?>) <$> traverse tangle g <*> tangle t
  tangle Blank = get >>= \ i -> Var (uv i) <$ put (i + 1)
  tangle (String k ws z) = String k <$> traverse (traverse tangle) ws <*> pure z
  tangle (Cell s t) = Cell <$> tangle s <*> tangle t
  tangle (App f as) = App <$> tangle f <*> traverse tangle as
  tangle (Semi s t) = Semi <$> tangle s <*> tangle t
  tangle (Prio s t) = Prio <$> tangle s <*> tangle t
  tangle (Match p t) = Match p <$> tangle t
  tangle t = pure t

