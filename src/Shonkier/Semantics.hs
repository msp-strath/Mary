module Shonkier.Semantics where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Maybe (fromMaybe)
import Data.Map (singleton, (!?), keysSet)
import qualified Data.Text as T

import Data.Bwd
import Shonkier.Syntax
import Shonkier.Scope
import Shonkier.Value
import Shonkier.Primitives (prim)

-- environments

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

runShonkier :: Shonkier a -> GlobalEnv -> Context -> (a, Context)
runShonkier m gl s = (`runReader` gl) . (`runStateT` s) $ getShonkier m

evalShonkier :: Shonkier a -> GlobalEnv -> Context -> a
evalShonkier m gl s = fst $ runShonkier m gl s

execShonkier :: Shonkier a -> GlobalEnv -> Context -> Context
execShonkier m gl s = snd $ runShonkier m gl s

shonkier :: GlobalEnv -> Term -> Computation
shonkier rho t = evalShonkier (eval (mempty, t)) rho Nil

rawShonkier :: [Import] -> GlobalEnv -> RawTerm -> Computation
rawShonkier is gl t =
  let scope = fmap keysSet gl
      term  = checkRaw "." is scope t
  in shonkier gl term

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

globalLookup :: FilePath -> Variable -> Shonkier (Maybe Value)
globalLookup fp x = do
  st <- ask
  pure $ do
    candidates <- st !? x
    candidates !? fp

eval :: (LocalEnv, Term) -> Shonkier Computation
eval (rho, t) = case t of
  Var x     -> case x of
    -- successes
    LocalVar x     -> use (fromMaybe theIMPOSSIBLE $ rho !? x)
    GlobalVar fp x -> do v <- globalLookup fp x
                         use (fromMaybe theIMPOSSIBLE v)
    -- error cases
    AmbiguousVar _ x     -> handle ("AmbiguousVar", [vVar x]) []
    OutOfScope x         -> handle ("OutOfScope", [vVar x]) []
    InvalidNamespace _ x -> handle ("InvalidNamespace", [vVar x]) []
  -- move left; start evaluating left to right
  Atom a    -> use (VAtom a)
  Lit l     -> use (VLit l)
  Cell a b  -> do push (CellL rho b)
                  eval (rho, a)
  App f as  -> do push (AppL rho as)
                  eval (rho, f)
  Fun es cs -> use (VFun [] rho es cs)

  where theIMPOSSIBLE = error "The IMPOSSIBLE happened!"
        vVar x = VString "" $ T.pack x

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
    -> Bwd Computation -> LocalEnv -> [([String],Term)]
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

call :: LocalEnv -> [Clause] -> [Computation]
     -> Shonkier Computation
call rho []                cs = do
  st <- get
  handle ("IncompletePattern", []) []
call rho ((ps, rhs) : cls) cs = case matches cmatch ps cs of
  Nothing  -> call rho cls cs
  Just sig -> eval (merge rho sig, rhs)


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
