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
import Utils.List

---------------------------------------------------------------------------
-- EXCEPTIONS (ESPECIALLY ABORT)
---------------------------------------------------------------------------

-- Shonkier.Value defines abortA

abort :: Shonkier Computation
abort = complain abortA []

-- how to complain

complain :: Atom -> [Value] -> Shonkier Computation
complain c vs = request c vs

---------------------------------------------------------------------------
-- PATTERN MATCHING
---------------------------------------------------------------------------

lmatch :: Literal -> Literal -> Maybe ()
lmatch x y = guard (x == y)

vmatch :: Eq a => PValue' a -> Value' a v -> Maybe (LocalEnv' a v)
vmatch (PAtom a)   (VAtom b)   = mempty <$ guard (a == b)
vmatch (PLit l)    (VLit l')   = mempty <$ lmatch l l'
vmatch (PBind x)   v           = pure (singleton x v)
vmatch (PAs x p)   v           = merge (singleton x v) <$> vmatch p v
vmatch PWild       v           = pure mempty
vmatch PNil        VNil        = pure mempty
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
      VFun frs mempty []
        [([PValue (PBind "_return")],
          [Nothing :?> Var (LocalVar :.: "_return")])]
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
  PNil    -> return (B0, mempty, t0)
  PCell p q -> do
    (tz1, rho1, t1) <- tmatch p Head t0
    (tz2, rho2, t2) <- tmatch q d t1
    return (tz1 <> tz2, merge rho1 rho2, t2)
  PAtom a -> return (B0, mempty, t0)
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
    return (B0 :< l, rho, r)

matches :: (a -> b -> Maybe (LocalEnv' c d))
        -> [a] -> [b] -> Maybe (LocalEnv' c d)
matches match as bs = foldl merge mempty <$> mayZipWith match as bs

---------------------------------------------------------------------------
-- ENTRY POINTS
---------------------------------------------------------------------------

runShonkier :: Shonkier a -> Env -> Context -> (a, Context)
runShonkier m gl s = (`runReader` gl) . (`runStateT` s) $ getShonkier m

evalShonkier :: Shonkier a -> Env -> Context -> a
evalShonkier m gl s = fst $ runShonkier m gl s

execShonkier :: Shonkier a -> Env -> Context -> Context
execShonkier m gl s = snd $ runShonkier m gl s

shonkier :: Env -> Term -> Computation
shonkier rho t = evalShonkier (eval (mempty, t)) rho CxNil

rawShonkier :: [Import] -> FilePath -> Env -> RawTerm -> Computation
rawShonkier is fp env@(gl, ins) t =
  let scope = fmap keysSet gl
      term  = checkRaw fp is scope t
  in shonkier env term

---------------------------------------------------------------------------
-- LOOKUP
---------------------------------------------------------------------------

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

dynVar :: ScopedVariable -> Maybe Variable
dynVar (sco :.: x) = x <$ guard (dyn sco) where
  dyn (GlobalVar True _)    = False
  dyn (InvalidNamespace _)  = False
  dyn _                     = True

---------------------------------------------------------------------------
-- EVALUATION
---------------------------------------------------------------------------

eval :: (LocalEnv, Term) -> Shonkier Computation
eval (rho, t) = case t of
  Var x@(sco :.: x') -> case dynVar x >>= (rho !?) of
    Just v  -> use v
    Nothing -> case sco of
      -- successes
      LocalVar       -> theIMPOSSIBLE "LocalVar"
      GlobalVar _ fp -> do
        v <- globalLookup fp x'
        use (fromMaybe (theIMPOSSIBLE "GlobalVar") v)

      -- error cases
      OutOfScope         -> complain "OutOfScope" [vVar x']
      AmbiguousVar _     -> complain "AmbiguousVar" [vVar x']
      InvalidNamespace _ -> complain "InvalidNamespace" [vVar x']
  -- move left; start evaluating left to right
  Atom a    -> use (VAtom a)
  Lit l     -> use (VLit l)
  Nil       -> use VNil
  Blank     -> complain "UnboundBlank" []
  Cell a b  -> do push (CellL rho b)
                  eval (rho, a)
  App f as  -> do push (AppL rho as)
                  eval (rho, f)
  Semi l r  -> do -- push (SemiL rho r)
                  push (AppL rho [r])
                  eval (rho, l)
  Prio l r  -> do push (PrioL rho r)
                  eval (rho, l)
  Fun es cs -> use (VFun CnNil rho es (braceFun cs))
  String k sts u -> case sts of
    []           -> use (VString k u)
    (s, t) : sts -> do
      push (StringLR (VString k s) rho sts u)
      eval (rho, t)
  Match p t -> do
    push (MatchR p)
    eval (rho, t)
  -- premature optimization is the root of all evil
  Mask "abort" t -> do
    gets cxHand >>= \case
      Right (ctx@(Cx hz fz), fr, gz)
        | case fr of { PrioL _ _ -> True ; Clauses{} -> True ; _ -> False }
        -> put (Cx hz (fz <> gz))
      _ -> push (Masking "abort")
    eval (rho, t)
  -- back to the usual
  Mask a t -> do
    push (Masking a)
    eval (rho, t)

  where
    theIMPOSSIBLE x = error $ "The IMPOSSIBLE happened! " ++ x
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
      VLit (Boolean b)
        | b -> case as of
            [t] -> eval (rho, t)
            _   -> complain "GuardsAreUnary" []
        | otherwise -> abort
      VAtom f ->
        -- Here we are enforcing the invariant:
        -- Atomic functions i.e. requests only ever offer
        -- to handle the empty list of requests.
        let cs = map ([],) as
        in app (FAtom f) B0 rho cs
      VPrim f hs          ->
        let cs = zip (hs ++ repeat []) as
        in app (FPrim f) B0 rho cs
      VFun frs sig hs cls -> do
        let cs = zip (hs ++ repeat []) as
        cont frs
        app (FFun {-frs-} sig cls) B0 rho cs
      VThunk c -> case as of
        [] -> case c of
          Value v      -> use v
          Request r k  -> handle r k
        _  -> complain "ThunksAreNullary" [v]
      v -> case as of
        [t] -> eval (merge rho (value2env v), t)
        _   -> complain "EnvironmentsAreUnary" []
    AppR f vz (_, rho) as -> app f (vz :< Value v) rho as
    SemiL rho r -> eval (rho, r)
    PrioL _ _ -> use v
    StringLR p rho sts u -> case sts of
      [] -> glom (VCell p (VCell v (VString "" u)))
      ((s, t) : sts) -> do
        push (StringLR (VCell p (VCell v (VString "" s))) rho sts u)
        eval (rho, t)
    MatchR p -> case vmatch p v of
      Nothing  -> abort
      Just sig -> use (VEnv sig)
    Masking _ -> use v
    Clauses{} -> use v

app :: Funy
    -> Bwd Computation -> LocalEnv -> [([Atom],Term)]
    -> Shonkier Computation
app f cz rho = \case
  []             -> case f of
    FAtom a         ->
      -- Here we are relying on the invariant:
      -- Atomic functions i.e. requests only ever offer
      -- to handle the empty list of requests.
      let vs = map unsafeComToValue (cz <>> []) in
      request a vs
    FPrim p         -> prim p (cz <>> [])
    FFun {-frs-} sig cs -> {- do cont frs -}
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

handleInput :: Request -> Continuation -> Shonkier Computation
handleInput (a, vs) k = case vs of
  [VString _ f] -> do
    -- preprocess the array key depending on the effect
    let f' = case a of
          "POST" -> "POST_" <> f
          "GET" -> "GET_"  <> f
          _       -> f
    inputLookup f' >>= \case
      Just v -> do
        cont k
        use v
      Nothing -> do cont k; complain "UnknownInput" vs
  _             -> do cont k; complain "IncorrectInputRequest" vs

handle :: Request
       -> Continuation
       -> Shonkier Computation
handle r@(a, _) k = go (Right k) 0 where
  -- do we get away with not making the number part of the request?
  go x i = leap x >>= \case
    Left k
      | a `elem` ["POST", "GET", "meta"] && i == 0 -> handleInput r k
      | otherwise -> return (Request r k)
    Right (hs, (fr, k)) -> case fr of
      AppR f cz (es, rho) as | a `elem` es ->
        if i == 0
          then app f (cz :< Request r k) rho as
          else go (Left hs) (i - 1)
      PrioL rho r | a == "abort" ->
        if (i == 0)
          then eval (rho, r)
          else go (Left hs) (i - 1)
      Clauses rho cls cs | a == "abort" ->
        if (i == 0)
          then call rho cls cs
          else go (Left hs) (i - 1)
      Masking b | a == b -> go (Left hs) (i + 1)
      _ -> go (Left hs) i

-- used for atomic requests
request :: Atom -> [Value] -> Shonkier Computation
request a vs = handle (a, vs) CnNil

call :: LocalEnv -> [Clause] -> [Computation]
     -> Shonkier Computation
call rho []                cs = abort
call rho ((ps, rhs) : cls) cs = case matches cmatch ps cs of
  Nothing  -> call rho cls cs
  Just sig -> do
    push (Clauses rho cls cs)
    eval (merge rho sig, rhs2Term rhs)


---------------------------------------------------------------------------
-- GLOMMING TEXT FROM VALUES
---------------------------------------------------------------------------

glom :: Value -> Shonkier Computation
glom v = go Nothing B0 [v] where
  go mk tz vs = case vs of
    [] ->
      use (VString (fromMaybe "" mk) (T.concat $ (tz <>> [])))
    VCell a b   : vs -> go mk tz (a : b : vs)
    VAtom _     : vs -> go mk tz vs
    VNil        : vs -> go mk tz vs
    VString k t : vs -> go (mk <|> pure k) (tz :< t) vs
    v           : _  -> complain "Invalid_StringConcat_ArgType" [v]
