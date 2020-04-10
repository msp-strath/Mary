module Shonkier.Semantics where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

import Data.Maybe (fromMaybe)
import Data.Map (singleton, (!?), keysSet)
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text (Text)
import qualified Data.Text as T

import Data.Bwd
import Shonkier.Syntax
import Shonkier.Scope
import Shonkier.Value
import Shonkier.Primitives (prim)

-- environments

lmatch :: Literal -> Literal -> Maybe ()
-- lmatch (String _ str) (String _ str') = guard (str == str')
lmatch (Num q)        (Num q')        = guard (q == q')
-- lmatch _ _ = Nothing

vmatch :: Eq a => PValue' a -> Value' a v -> Maybe (LocalEnv' a v)
vmatch (PAtom a)   (VAtom b)   = mempty <$ guard (a == b)
vmatch (PLit l)    (VLit l')   = mempty <$ lmatch l l'
vmatch (PBind x)   v           = pure (singleton x v)
vmatch (PAs x p)   v           = merge (singleton x v) <$> vmatch p v
vmatch PWild       v           = pure mempty
vmatch (PCell p q) (VCell v w) = merge <$> vmatch p v <*> vmatch q w
vmatch (PString _ sps u) (VString _ t) | Just p <- T.stripSuffix u t =
  smatch sps p
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

smatch :: Eq a => [(Text, PValue' a)] -> Text -> Maybe (LocalEnv' a v)
smatch [] t = mempty <$ guard (T.null t)
smatch ((d, p) : dps) t = do
  t <- T.stripPrefix d t
  let d' = case dps of { [] -> ToEnd ; (d, _) : _ -> ToDelim d }
  (_, rho, t) <- tmatch p d' t
  merge rho <$> smatch dps t

data MaPo -- lazy patterns match differently depending on their position
  = Head            -- in a head position, they match one character
  | ToEnd           -- in an undelimited tail position, they match the whole text
  | ToDelim Text  -- in a delimited tail position, they match until the delimiter

tmatch :: Eq a
       => PValue' a   -- pattern in tail position
       -> MaPo        -- match position
       -> Text        -- text
       -> Maybe ( Bwd Text       -- segments consumed, in case of as-pattern
                , LocalEnv' a v  -- matching environment
                , Text           -- rest of text
                )
tmatch p d t0 = case p of
  PAs x p   -> do
    (tz, rho, t1) <- tmatch p d t0
    let v = VString "" (T.concat (tz <>> []))
    return (tz, merge (singleton x v) rho, t1)
  PCell p q -> do
    (tz1, rho1, t1) <- tmatch p Head t0
    (tz2, rho2, t2) <- tmatch q d t1
    return (tz1 <> tz2, merge rho1 rho2, t2)
  PAtom a -> return (Nil, mempty, t0)
  PLit _  -> Nothing
  -- otherwise, it's a lazy string pattern, and we need the MaPo logic...
  _ -> do
    (l, r) <- case d of  -- ...to figure out how to cut up the text
      Head -> do
        (c, t1) <- T.uncons t0
        return (T.cons c "", t1)
      ToEnd -> return (t0, "")
      ToDelim d | T.null d  -> return ("", t0)
                | otherwise -> return (T.breakOn d t0)
    rho <- vmatch p (VString "" l)
    return (Nil :< l, rho, r)

mayZipWith :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
mayZipWith f []       []       = pure []
mayZipWith f (a : as) (b : bs) =
  (:) <$> f a b <*> mayZipWith f as bs
mayZipWith _ _ _ = Nothing

matches :: (a -> b -> Maybe (LocalEnv' c d))
        -> [a] -> [b] -> Maybe (LocalEnv' c d)
matches match as bs = foldl merge mempty <$> mayZipWith match as bs

-- Evaluation contexts

runShonkier :: Shonkier a -> Env -> Context -> (a, Context)
runShonkier m gl s = (`runReader` gl) . (`runStateT` s) $ getShonkier m

evalShonkier :: Shonkier a -> Env -> Context -> a
evalShonkier m gl s = fst $ runShonkier m gl s

execShonkier :: Shonkier a -> Env -> Context -> Context
execShonkier m gl s = snd $ runShonkier m gl s

shonkier :: Env -> Term -> Computation
shonkier rho t = evalShonkier (eval (mempty, t)) rho Nil

rawShonkier :: [Import] -> Env -> RawTerm -> Computation
rawShonkier is env@(gl, ins) t =
  let scope = fmap keysSet gl
      term  = checkRaw "." is scope t
  in shonkier env term

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
  (st, _) <- ask
  pure $ do
    candidates <- st !? x
    candidates !? fp

inputLookup :: Text -> Shonkier (Maybe Value)
inputLookup f = do
  (_, inp) <- ask
  pure $ VString "" <$> inp !? f

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
  Semi l r  -> do push (SemiL rho r)
                  eval (rho, l)
  Fun es cs -> use (VFun [] rho es cs)
  String k sts u -> case sts of
    []           -> use (VString k u)
    (s, t) : sts -> do
      push (StringLR (VString k s) rho sts u)
      eval (rho, t)

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
    SemiL rho r -> eval (rho, r)
    StringLR p rho sts u -> case sts of
      [] -> glom (VCell p (VCell v (VString "" u)))
      ((s, t) : sts) -> do
        push (StringLR (VCell p (VCell v (VString "" s))) rho sts u)
        eval (rho, t)

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

handleInput :: Request -> Shonkier Computation
handleInput (a, vs) = case vs of
  [VString _ f] -> do
    let f' = case a of
          "field" -> "POST_" <> f
          "param" -> "GET_"  <> f
          _       -> f
    mv <- inputLookup f'
    maybe (handle ("UnknownInput", vs) []) use mv
  _             -> handle ("IncorrectInputRequest", vs) []

handle :: Request -> [Frame]
       -> Shonkier Computation
handle r@(a, _) _ | a `elem` ["field", "param", "meta"] = handleInput r
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
-- GLOMMING TEXT FROM VALUES
---------------------------------------------------------------------------

glom :: Value -> Shonkier Computation
glom v = go Nothing Nil [v] where
  go mk tz vs = case vs of
    [] ->
      use (VString (fromMaybe "" mk) (T.concat $ (tz <>> [])))
    VCell a b   : vs -> go mk tz (a : b : vs)
    VAtom _     : vs -> go mk tz vs
    VString k t : vs -> go (mk <|> pure k) (tz :< t) vs
    v           : _  -> handle ("Invalid_StringConcat_ArgType", [v]) []


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
