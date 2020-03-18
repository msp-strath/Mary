{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Shonkier.Semantics where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.Either
import Data.Foldable
import Data.Function
import Data.Map (Map, singleton, (!?))
import qualified Data.Map as Map
import Data.List (sortBy, groupBy, nub)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Directory

import Data.Bwd
import Shonkier.Scope
import Shonkier.Syntax
import Shonkier.Parser

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

-- environments

type LocalEnv' a = Map Variable (Value' a)
type LocalEnv = LocalEnv' String

type GlobalEnv' a = Map Variable (Map FilePath (Value' a))
type GlobalEnv = GlobalEnv' String

merge :: LocalEnv' a -> LocalEnv' a -> LocalEnv' a
merge = flip (<>)

padCat :: Eq a => [[a]] -> [[a]] -> [[a]]
padCat [] hs = hs
padCat hs [] = hs
padCat (a : as) (b : bs) = (a ++ b) : padCat as bs

lmatch :: Literal -> Literal -> Maybe ()
lmatch (String _ str) (String _ str') = guard (str == str')
lmatch (Num q)        (Num q')        = guard (q == q')
lmatch _ _ = Nothing

vmatch :: Eq a => PValue' a -> Value' a -> Maybe (LocalEnv' a)
vmatch (PAtom a)   (VAtom b)   = mempty <$ guard (a == b)
vmatch (PLit l)    (VLit l')   = mempty <$ lmatch l l'
vmatch (PBind x)   v           = pure (singleton x v)
vmatch (PAs x p)   v           = merge (singleton x v) <$> vmatch p v
vmatch PWild       v           = pure mempty
vmatch (PCell p q) (VCell v w) = merge <$> vmatch p v <*> vmatch q w
vmatch _ _ = Nothing

cmatch :: PComputation -> Computation -> Maybe LocalEnv
cmatch (PValue p)           (Value v)             = vmatch p v
cmatch (PRequest (a, ps) k) (Request (b, vs) frs) = do
  guard (a == b)
  rho <- matches vmatch ps vs
  return $ merge rho $ case k of
    Nothing -> mempty
    Just k  -> singleton k $
      VFun frs mempty [] [([PValue (PBind "_return")], Var (LocalVar "_return"))]
cmatch (PThunk k) c = pure $ singleton k $ VThunk c
cmatch _ _ = Nothing

mayZipWith :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
mayZipWith f []       []       = pure []
mayZipWith f (a : as) (b : bs) =
  (:) <$> f a b <*> mayZipWith f as bs
mayZipWith _ _ _ = Nothing

matches :: (a -> b -> Maybe (LocalEnv' c))
        -> [a] -> [b] -> Maybe (LocalEnv' c)
matches match as bs = foldl merge mempty <$> mayZipWith match as bs

-- Evaluation contexts

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

data ShonkierState = ShonkierState
  { visited :: Set FilePath
  , globals :: GlobalEnv
  , context :: Context
  } deriving (Show)

emptyShonkierState :: ShonkierState
emptyShonkierState = ShonkierState
  { visited = Set.empty
  , globals = primEnv
  , context = Nil
  }

newtype ShonkierT m a = ShonkierT
  { getShonkierT :: StateT ShonkierState m a }
  deriving ( Functor, Applicative, Monad
           , MonadState ShonkierState, MonadIO)

runShonkierT :: Monad m => ShonkierT m a -> ShonkierState -> m (a, ShonkierState)
runShonkierT m s = (`runStateT` s) $ getShonkierT m

evalShonkierT :: Monad m => ShonkierT m a -> ShonkierState -> m a
evalShonkierT m s = fst <$> runShonkierT m s

execShonkierT :: Monad m => ShonkierT m a -> ShonkierState -> m ShonkierState
execShonkierT m s = snd <$> runShonkierT m s

forceImportModule :: FilePath -> ShonkierT IO Module
forceImportModule fp = do
  txt <- liftIO $ TIO.readFile fp
  loadModule fp $ getMeAModule txt

importModule :: FilePath -> ShonkierT IO (Maybe Module)
importModule fp = do
  cached <- gets (Set.member fp . visited)
  exists <- liftIO $ doesFileExist fp
  if (cached || not exists)
    then pure Nothing
    else Just <$> forceImportModule fp

loadModule :: FilePath -> RawModule -> ShonkierT IO Module
loadModule fp (is, ls) = do
  mapM_ importModule is
  scope <- gets (fmap Map.keysSet . globals)
  let ps = checkProgram fp (Set.fromList ("." : fp : is)) scope ls
  let env = mkGlobalEnv fp ps
  modify (\ r -> r { globals = Map.unionWith (<>) (globals r) env
                   , visited = Set.insert fp (visited r)
                   })
  pure (is, ps)

mkGlobalEnv :: FilePath -> Program -> GlobalEnv
mkGlobalEnv fp ls = fold
  [ singleton f $ singleton fp
    $ VFun [] mempty
      (map nub (foldr padCat [] [hs | (_, Left hs) <- grp]))
      [cl | (_, Right cl) <- grp]
  | grp@((f, _) : _) <- groupBy ((==) `on` fst) $
                     -- Note: sortBy is stable
                        sortBy (compare `on` (id *** isRight)) $
                        ls
  ]

shonkier :: GlobalEnv -> Term -> Computation
shonkier rho t =
  runIdentity $ evalShonkierT (eval (mempty, t)) state where
  state = emptyShonkierState { globals = rho }

push :: Monad m => Frame -> ShonkierT m ()
push fr = modify (\ r -> r { context = context r :< fr })

pop :: Monad m => ShonkierT m (Maybe Frame)
pop = do
  x <- gets context
  case x of
    Nil -> return Nothing
    ctx :< fr -> do
      modify (\ r -> r { context = ctx })
      return (Just fr)

globalLookup :: Monad m => FilePath -> Variable -> ShonkierT m (Maybe Value)
globalLookup fp x = do
  st <- get
  pure $ do
    candidates <- globals st !? x
    candidates !? fp

eval :: Monad m => (LocalEnv, Term) -> ShonkierT m Computation
eval (rho, t) = case t of
  Var x     -> case x of
    LocalVar x     -> use (fromMaybe (error $ "The IMPOSSIBLE happened!") $ rho !? x)
    GlobalVar fp x -> do v <- globalLookup fp x
                         use (fromMaybe (error "The IMPOSSIBLE happened!") v)
    AmbiguousVar{} -> handle ("AmbiguousName", []) []
    OutOfScope{}   -> handle ("OutOfScope", []) []
  -- move left; start evaluating left to right
  Atom a    -> use (VAtom a)
  Lit l     -> use (VLit l)
  Cell a b  -> do push (CellL rho b)
                  eval (rho, a)
  App f as  -> do push (AppL rho as)
                  eval (rho, f)
  Fun es cs -> use (VFun [] rho es cs)

use :: Monad m => Value -> ShonkierT m Computation
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

app :: Monad m => Funy
    -> Bwd Computation -> LocalEnv -> [([String],Term)]
    -> ShonkierT m Computation
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

handle :: Monad m => Request -> [Frame]
       -> ShonkierT m Computation
handle r@(a, vs) frs = pop >>= \case
  Nothing         -> return (Request r frs)
  Just fr -> case fr of
    AppR f cz (hs, rho) as | a `elem` hs ->
      app f (cz :< Request r frs) rho as
    _ -> handle r (fr : frs)

call :: Monad m => LocalEnv -> [Clause] -> [Computation]
     -> ShonkierT m Computation
call rho []                cs = do
  st <- get
  handle ("IncompletePattern", []) []
call rho ((ps, rhs) : cls) cs = case matches cmatch ps cs of
  Nothing  -> call rho cls cs
  Just sig -> eval (merge rho sig, rhs)


---------------------------------------------------------------------------
-- PRIMITIVES
---------------------------------------------------------------------------

primEnv :: GlobalEnv
primEnv = foldMap toVal (primitives :: [(Primitive, PRIMITIVE Identity)]) where
  toVal (str, _) = singleton str $ singleton "." $ VPrim str []

prim :: Monad m => Primitive -> [Computation] -> ShonkierT m Computation
prim nm vs = case lookup nm primitives of
  Nothing -> handle ("NoPrim",[VPrim nm []]) []
  Just f  -> f vs

type PRIMITIVE m = [Computation] -> ShonkierT m Computation

primitives :: Monad m => [(Primitive, PRIMITIVE m)]
primitives =
  [ ("primStringConcat", primStringConcat)
  , ("primNumAdd"      , primNumAdd)
  , ("primNumMinus"    , primNumMinus)
  , ("primNumMult"     , primNumMult)
  ]

---------------------------------------------------------------------------
-- NUM

primNumBin :: Monad m => String -> (Rational -> Rational -> Rational)
           -> PRIMITIVE m
primNumBin nm op = \case
  [CNum m, CNum n]   -> use (VNum (op m n))
  [Value m, Value n] -> handle ("Invalid_" ++ nm ++ "_ArgType", [m, n]) []
  [_,_]              -> handle ("Invalid_" ++ nm ++ "Add_ArgRequest",[]) []
  _                  -> handle ("Invalid_" ++ nm ++ "_Arity", []) []

primNumAdd, primNumMinus, primNumMult :: Monad m => PRIMITIVE m
primNumAdd   = primNumBin "primNumAdd" (+)
primNumMinus = primNumBin "primNumMinus" (-)
primNumMult  = primNumBin "primNumMult" (*)

---------------------------------------------------------------------------
-- STRING

primStringConcat :: forall m. Monad m => PRIMITIVE m
primStringConcat cs = go cs Nothing [] where

  go :: [Computation] -> Maybe Keyword -> [Text] -> ShonkierT m Computation
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

instance (FromValue a, FromValue b) => FromValue (a, b) where
  fromValue (VCell a b) = (fromValue a, fromValue b)
  fromValue _ = (fromValue VNil, fromValue VNil)

instance (FromValue a, FromValue b, FromValue c) => FromValue (a, b, c) where
  fromValue (VCell a (VCell b c)) = (fromValue a, fromValue b, fromValue c)
  fromValue _ = (fromValue VNil, fromValue VNil, fromValue VNil)
