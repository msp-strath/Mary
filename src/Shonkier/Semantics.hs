{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shonkier.Semantics where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Foldable
import Data.Function
import Data.Either
import Data.Map (Map, singleton, (!?))
import Data.List (sortBy, groupBy, nub)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text (Text)
import qualified Data.Text as T

import Data.Bwd
import Shonkier.Syntax


data Value' a
  = VAtom a
  | VLit Literal
  | VPrim Primitive [[a]]
  | VCell (Value' a) (Value' a)
  | VFun [Frame' a] (Env' a) [[a]] [Clause' a]
  -- ^ Env is the one the function was created in
  --   Frames ??
  | VThunk (Computation' a)
  deriving (Show)

type Value = Value' String

pattern VNum n        = VLit (Num n)
pattern CNum n        = Value (VNum n)
pattern VString k str = VLit (String k str)
pattern CString k str = Value (VString k str)
pattern CCell a b     = Value (VCell a b)
pattern CAtom a       = Value (VAtom a)

-- environments

type Env' a = Map String (Value' a)
type Env = Env' String

merge :: Env' a -> Env' a -> Env' a
merge = flip (<>)

mkGlobalEnv :: Program -> Env
mkGlobalEnv ls = primEnv <> fold
  [ singleton f $
     VFun [] mempty
       (map nub (foldr padCat [] [hs | (_, Left hs) <- grp]))
       [cl | (_, Right cl) <- grp]
  | grp@((f, _) : _) <- groupBy ((==) `on` fst) $
                        -- Note: sortBy is stable
                        sortBy (compare `on` (id *** isRight)) $
                        ls
  ]

padCat :: Eq a => [[a]] -> [[a]] -> [[a]]
padCat [] hs = hs
padCat hs [] = hs
padCat (a : as) (b : bs) = (a ++ b) : padCat as bs

lmatch :: Literal -> Literal -> Maybe ()
lmatch (String _ str) (String _ str') = guard (str == str')
lmatch (Num q)        (Num q')        = guard (q == q')
lmatch _ _ = Nothing

vmatch :: Eq a => PValue' a -> Value' a -> Maybe (Env' a)
vmatch (PAtom a)   (VAtom b)   = mempty <$ guard (a == b)
vmatch (PLit l)    (VLit l')   = mempty <$ lmatch l l'
vmatch (PBind x)   v           = pure (singleton x v)
vmatch PWild       v           = pure mempty
vmatch (PCell p q) (VCell v w) = merge <$> vmatch p v <*> vmatch q w
vmatch _ _ = Nothing

cmatch :: Eq a => PComputation' a -> Computation' a -> Maybe (Env' a)
cmatch (PValue p)           (Value v)             = vmatch p v
cmatch (PRequest (a, ps) k) (Request (b, vs) frs) = do
  guard (a == b)
  rho <- matches vmatch ps vs
  return $ merge rho $ singleton k $
    VFun frs mempty [] [( [PValue (PBind "_return")]
                        , Var "_return"
                        )]
cmatch (PThunk k) c = pure $ singleton k $ VThunk c
cmatch _ _ = Nothing

mayZipWith :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
mayZipWith f []       []       = pure []
mayZipWith f (a : as) (b : bs) =
  (:) <$> f a b <*> mayZipWith f as bs
mayZipWith _ _ _ = Nothing

matches :: (a -> b -> Maybe (Env' c)) -> [a] -> [b] -> Maybe (Env' c)
matches match as bs = foldl merge mempty <$> mayZipWith match as bs

-- Evaluation contexts

data Funy' a
  = FAtom a
  | FPrim Primitive
  | FFun [Frame' a] (Env' a) [Clause' a]
  deriving (Show)
type Funy = Funy' String

-- The argument of type (Env' a) indicates the
-- cursor position
data Frame' a
  = CellL (Env' a) (Term' a)
  | CellR (Value' a) (Env' a)
  | AppL (Env' a) [Term' a]
  | AppR (Funy' a)
         (Bwd (Computation' a))
         -- ^ already evaluated arguments (some requests we are
         --   willing to handle may still need to be dealt with)
         ([a], Env' a)
         -- ^ focus: [a] = requests we are willing to handle
         [([a],Term' a)]
         -- ^ each arg comes with requests we are willing to handle
  deriving (Show)

type Frame = Frame' String

type Context' a = Bwd (Frame' a)
type Context = Context' String

-- Evaluation functions

type Request' a = (a, [Value' a])
type Request = Request' String

data Computation' a
  = Value (Value' a)
  | Request (Request' a) [Frame' a]
  -- ^ Invoking an effect & none of the
  -- frames present know how to interpret it
  deriving (Show)

type Computation = Computation' String

newtype Shonkier a = Shonkier { runShonkier :: StateT Context (Reader Env) a }
  deriving (Functor, Applicative, Monad, MonadState Context, MonadReader Env)

shonkier :: Env -> Term -> Computation
shonkier rho t = runReader (evalStateT (runShonkier $ eval (mempty, t)) Nil) rho

push :: Frame -> Shonkier ()
push fr = modify (:< fr)

pop :: Shonkier (Maybe Frame)
pop = do
  x <- get
  case x of
    Nil -> return Nothing
    ctx :< fr -> do
      put ctx
      return (Just fr)

globalLookup :: String -> Shonkier (Maybe Value)
globalLookup n = asks (!? n)

eval :: (Env, Term) -> Shonkier Computation
eval (rho, t) = case t of
  Var x     -> case rho !? x of
    Just v  -> use v
    Nothing -> globalLookup x >>= \case
      Just v  -> use v
      Nothing -> handle ("OutOfScope", []) []
  -- move left; start evaluating left to right
  Atom a    -> use (VAtom a)
  Lit l     -> use (VLit l)
  Cell a b  -> do push (CellL rho b)
                  eval (rho, a)
  App f as  -> do push (AppL rho as)
                  eval (rho, f)
  Fun es cs -> use (VFun [] rho es cs)

use :: Value -> Shonkier Computation
use v = pop >>= \case
  Nothing -> return (Value v)
  Just fr -> case fr of
    -- move right or upwards
    CellL rho b -> do push (CellR v rho)
                      eval (rho, b)
    CellR u rho -> use (VCell u v)
    -- we better be making a request or using a function
    AppL rho as -> case v of
      VAtom f            ->
        -- Here we are enforcing the invariant:
        -- Atomic functions i.e. requests only ever offer
        -- to handle the empty list of requests.
        let cs = map ([],) as
        in app (FAtom f) Nil rho cs
      VPrim f hs          ->
        let cs = zip (hs ++ repeat []) as
        in app (FPrim f) Nil rho cs
      VFun frs sig hs cls ->
        let cs = zip (hs ++ repeat []) as
        in app (FFun frs sig cls) Nil rho cs
      VThunk c -> case as of
        [] -> case c of
          Value v       -> use v
          Request r frs -> handle r frs
        _  -> handle ("ThunksAreNullary", [v]) []
      _ -> handle ("NoFun",[v]) []
    AppR f vz (_, rho) as -> app f (vz :< Value v) rho as

app :: Funy
    -> Bwd Computation -> Env -> [([String],Term)]
    -> Shonkier Computation
app f cz rho = \case
  []             -> case f of
    FAtom a         ->
      -- Here we are relying on the invariant:
      -- Atomic functions i.e. requests only ever offer
      -- to handle the empty list of requests.
      let vs = map unsafeComToValue (cz <>> []) in
      handle (a, vs) []
    FPrim p         -> prim p (cz <>> [])
    FFun frs sig cs -> do traverse push frs
                          call sig cs (cz <>> [])
  ((hs, a) : as) -> do push (AppR f cz (hs, rho) as)
                       eval (rho, a)

unsafeComToValue :: Computation -> Value
unsafeComToValue = \case
  Value v     -> v
  r@Request{} -> error $ unlines
    [ "ARGH! Feeding a request to a request"
    , show r
    ]

handle :: Request -> [Frame]
       -> Shonkier Computation
handle r@(a, vs) frs = pop >>= \case
  Nothing         -> return (Request r frs)
  Just fr -> case fr of
    AppR f cz (hs, rho) as | a `elem` hs ->
      app f (cz :< Request r frs) rho as
    _ -> handle r (fr : frs)

call :: Env -> [Clause] -> [Computation]
     -> Shonkier Computation
call rho []                cs =
  handle ("IncompletePattern", []) []
call rho ((ps, rhs) : cls) cs = case matches cmatch ps cs of
  Nothing  -> call rho cls cs
  Just sig -> eval (merge rho sig, rhs)


---------------------------------------------------------------------------
-- PRIMITIVES
---------------------------------------------------------------------------

primEnv :: Env
primEnv = foldMap (\ str -> singleton str $ VPrim str [])
        [ "primStringConcat"
        , "primNumAdd"
        ]

prim :: Primitive -> [Computation] -> Shonkier Computation
prim "primStringConcat" vs = primStringConcat vs
prim "primNumAdd"       vs = primNumAdd vs
prim f _ = handle ("NoPrim",[VPrim f []]) []

---------------------------------------------------------------------------
-- NUM

primNumAdd :: [Computation] -> Shonkier Computation
primNumAdd = \case
  [CNum m, CNum n]   -> use (VNum (m + n))
  [Value m, Value n] -> handle ("Invalid_NumAdd_ArgType", [m, n]) []
  [_,_]              -> handle ("Invalid_NumAdd_ArgRequest",[]) []
  _                  -> handle ("Invalid_NumAdd_Arity", []) []

---------------------------------------------------------------------------
-- STRING

primStringConcat :: [Computation] -> Shonkier Computation
primStringConcat cs = go cs Nothing [] where

  go :: [Computation] -> Maybe Keyword -> [Text] -> Shonkier Computation
  go cs mk ts = case cs of
    []                 -> let txt = T.concat $ reverse ts
                          in use (VString (fromMaybe "" mk) txt)
    (CString k t : cs) -> go cs (mk <|> pure k) (t:ts)
    (CCell a b   : cs) -> go (Value a : Value b : cs) mk ts
    (CAtom {}    : cs) -> go cs mk ts
    (Value v     : cs) -> handle ("Invalid_StringConcat_ArgType", [v]) []
    _                  -> handle ("Invalid_StringConcat_ArgRequest",[]) []

---------------------------------------------------------------------------
-- FROMVALUE
---------------------------------------------------------------------------

class FromValue t where
  fromValue :: Value -> t

instance FromValue t => FromValue [t] where
  fromValue (VCell t ts) = fromValue t : fromValue ts
  fromValue _ = []
