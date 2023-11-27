module Shonkier.Value where

import Control.Monad (guard)
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Data.Map (Map, singleton, toAscList)
import Data.Text (Text)

import Data.Bwd
import Data.Lisp
import Shonkier.Syntax
import Utils.List


---------------------------------------------------------------------------
-- ABORT
---------------------------------------------------------------------------

-- how to signal a failure

abortA :: Atomy a => a
abortA = atomOf "abort"


---------------------------------------------------------------------------
-- RHS TRANSLATION
---------------------------------------------------------------------------

rhs2Term :: Atomy a => [Rhs' a v] -> Term' a v
rhs2Term [] = App (Atom abortA) []
rhs2Term [Nothing :?> t] = t
rhs2Term ((mg :?> t) : rs) = Prio (guardBy mg (Mask abortA t)) (rhs2Term rs)
  where
  guardBy Nothing  t = t
  guardBy (Just g) t = App g [t]


---------------------------------------------------------------------------
-- ENVIRONMENTS
---------------------------------------------------------------------------

type Env = (GlobalEnv, Map Text Text) -- map for form data

type GlobalEnv' a v = Map Variable (Map FilePath (Value' a v))
type GlobalEnv = GlobalEnv' Atom ScopedVariable

type LocalEnv' a v = Map Variable (Value' a v)
type LocalEnv = LocalEnv' Atom ScopedVariable

merge :: LocalEnv' a v -> LocalEnv' a v -> LocalEnv' a v
merge = flip (<>)

---------------------------------------------------------------------------
-- VALUES
---------------------------------------------------------------------------

data Value' a v
  = VAtom a
  | VLit Literal
  | VNil
  | VCell (Value' a v) (Value' a v)
  | VString Keyword Text
  | VPrim Primitive [[a]]
  | VFun (Continuation' a v) (LocalEnv' a v) [[a]] [Clause' a v]
  -- ^ Env is the one the function was created in
  --   Frames ??
  | VThunk (Computation' a v)
  | VEnv (LocalEnv' a v)
  deriving (Show, Functor)

type Value = Value' Atom ScopedVariable

pattern VNum n        = VLit (Num n)
pattern CNum n        = Value (VNum n)
pattern VBoolean b    = VLit (Boolean b)
pattern CBoolean b    = Value (VBoolean b)
pattern CString k str = Value (VString k str)
pattern CCell a b     = Value (VCell a b)
pattern CAtom a       = Value (VAtom a)
pattern CNil          = Value VNil

---------------------------------------------------------------------------
-- FIRST-ORDER EQUALITY
---------------------------------------------------------------------------

hoValue :: Value -> Bool
hoValue (VPrim{})  = True
hoValue (VFun{})   = True
hoValue (VThunk{}) = True
hoValue _          = False

valueEqHuh :: Value -> Value -> Maybe Bool
valueEqHuh (VAtom a)     (VAtom b)     = Just (a == b)
valueEqHuh (VLit a)      (VLit b)      = Just (a == b)
valueEqHuh VNil          VNil          = Just True
valueEqHuh (VCell a b)   (VCell c d)   = (&&)
  <$> valueEqHuh a c
  <*> valueEqHuh b d
valueEqHuh (VString _ a) (VString _ b) = Just (a == b)
valueEqHuh (VEnv rho)    (VEnv sig)
  = all id <$> mayZipWith help (toAscList rho) (toAscList sig)
  where
  help (j, u) (k, v) = guard (j == k) *> valueEqHuh u v

valueEqHuh x y | hoValue x || hoValue y = Nothing
valueEqHuh _ _ = Just False


---------------------------------------------------------------------------
-- EXPLICIT ENVIRONMENTS
---------------------------------------------------------------------------

value2env :: Value -> LocalEnv
value2env (VEnv rho)          = rho
value2env (VCell (VAtom x) v) = singleton (getAtom x) v
value2env (VCell e1 e2)       = merge (value2env e2) (value2env e1)
value2env _                   = mempty


---------------------------------------------------------------------------
-- COMPUTATIONS
---------------------------------------------------------------------------

type Request' a v = (a, [Value' a v])
type Request = Request' Atom ScopedVariable

data Computation' a v
  = Value (Value' a v)
  | Request (Request' a v) (Continuation' a v) -- [Frame' a v]
  -- ^ Invoking an effect & none of the
  -- frames present know how to interpret it
  deriving (Show, Functor)

type Computation = Computation' Atom ScopedVariable

type HandList' a v =
  [ ( Frame' a v        -- handleFrame
    , Bwd (Frame' a v)  -- all (not . handleFrame)
    )
  ]

type HandList = HandList' Atom ScopedVariable

data Continuation' a v = Cn
  { aftard   :: Bwd (Frame' a v)   -- all (not . handleFrame)
  , handlers :: HandList' a v
  }
  deriving (Show, Functor)

type Continuation = Continuation' Atom ScopedVariable

pattern CnNil = Cn B0 []

cnFlat :: Continuation' a v -> [Frame' a v]
cnFlat (Cn fz hs) = fz <>> foldr (\ (f, fz) fs -> f : fz <>> fs) [] hs


---------------------------------------------------------------------------
-- EVALUATION CONTEXTS
---------------------------------------------------------------------------

data Funy' a v
  = FAtom a
  | FPrim Primitive
  | FFun {-(Continuation' a v)-} (LocalEnv' a v) [Clause' a v]
  deriving (Show, Functor)
type Funy = Funy' Atom ScopedVariable

-- The argument of type (LocalEnv' a) indicates the
-- cursor position
data Frame' a v
  = CellL (LocalEnv' a v) (Term' a v)
  | CellR (Value' a v) (LocalEnv' a v)  -- drop this env?
  | AppL (LocalEnv' a v) [Term' a v]
  | AppR (Funy' a v)
         (Bwd (Computation' a v))
         -- ^ already evaluated arguments (some requests we are
         --   willing to handle may still need to be dealt with)
         ([a], LocalEnv' a v)
         -- ^ focus: [a] = requests we are willing to handle
         [([a], Term' a v)]
         -- ^ each arg comes with requests we are willing to handle
  | SemiL (LocalEnv' a v) (Term' a v)
  | PrioL (LocalEnv' a v) (Term' a v)
  | StringLR (Value' a v) (LocalEnv' a v) [(Text, Term' a v)] Text
  | MatchR (PValue' a)
  | Masking a
  | Clauses (LocalEnv' a v) [Clause' a v] [Computation' a v]
  deriving (Show, Functor)

handleFrame :: Frame' a v -> Bool
handleFrame (PrioL _ _)           = True
handleFrame (AppR _ _ (_:_, _) _) = True
handleFrame (Masking a)           = True
handleFrame (Clauses{})       = True
handleFrame _ = False

type Frame = Frame' Atom ScopedVariable

data Context' a v = Cx
  { handlerz :: Bwd ( Bwd (Frame' a v)  -- all (not . handleFrame)
                    , Frame' a v        -- handleFrame
                    )
  , nandlerz :: Bwd (Frame' a v)        -- all (not . handleFrame)
  }
type Context = Context' Atom ScopedVariable

pattern CxNil = Cx B0 B0

cxNull :: Context' a v -> Bool
cxNull CxNil = True
cxNull _     = False

cxPush :: Context' a v -> Frame' a v -> Context' a v
cxPush (Cx hz fz) f
  | handleFrame f = Cx (hz :< (fz, f)) B0
  | otherwise     = Cx hz (fz :< f)

cxPop :: Context' a v -> Maybe (Context' a v, Frame' a v)
cxPop (Cx hz (fz :< f))        = Just (Cx hz fz, f)
cxPop (Cx (hz :< (fz, f)) B0)  = Just (Cx hz fz, f)
cxPop (Cx B0 B0) = Nothing

cxHand :: Context' a v -> Either
  ( Bwd (Frame' a v)   -- all (not . handleFrame)
  )
  ( Context' a v       -- whatever
  , Frame' a v         -- handleFrame
  , Bwd (Frame' a v)   -- all (not . handleFrame)
  )
cxHand (Cx (hz :< (fz, f)) gz) = Right (Cx hz fz, f, gz)
cxHand (Cx B0 gz)              = Left  gz

cxCn :: Context' a v -> Continuation' a v -> Context' a v
cxCn (Cx hz fz) (Cn gz hs) = foldl go (Cx hz (fz <> gz)) hs where
  go (Cx hz fz) (g , gz) = Cx (hz :< (fz, g)) gz


---------------------------------------------------------------------------
-- EVALUATION MONAD
---------------------------------------------------------------------------

newtype Shonkier a = Shonkier
  { getShonkier :: StateT Context (Reader GlobalEnv) a }
  deriving ( Functor, Applicative, Monad
           , MonadState Context, MonadReader GlobalEnv
           )

push :: Frame -> Shonkier ()
push fr = modify (`cxPush` fr)

pop :: Shonkier (Maybe Frame)
pop = gets cxPop >>= \case
  Nothing        -> return Nothing
  Just (ctx, fr) -> do
    put ctx
    return (Just fr)

leap :: Either HandList Continuation -> Shonkier
  (Either
    Continuation             -- unhandled effect has reached the world
    ( HandList               -- to keep searching, use this
    , (Frame, Continuation)  -- but maybe this frame handles you
    )
  )
leap x = gets cxHand >>= \case
  Left gz -> do
    put CxNil
    return . Left $ case x of
      Left   hs         -> (Cn gz hs)
      Right  (Cn fz hs) -> Cn (gz <> fz) hs
  Right (ctx, f, gz) -> do
    put ctx
    return . Right $ case x of
      Left hs          -> ((f , gz) : hs, (f, Cn gz hs))
      Right (Cn fz hs) -> let ez = gz <> fz in
        ((f , ez) : hs, (f, Cn ez hs))

cont :: Continuation -> Shonkier ()
cont k = modify (`cxCn` k)

---------------------------------------------------------------------------
-- INSTANCES
---------------------------------------------------------------------------

instance HasListView Value Value where
  coalgebra = \case
    VNil      -> ItsNil
    VCell a b -> ItsCons a b
    _         -> ItsNot

---------------------------------------------------------------------------
-- FROMVALUE
---------------------------------------------------------------------------

class FromValue t where
  fromValue :: Value -> Either Value t

instance FromValue Int where
  fromValue (VNum d) = pure (truncate d) -- TODO: check it is indeed an Int?
  fromValue v = Left v

instance FromValue Text where
  fromValue (VString _ s) = pure s
  fromValue v = Left v

instance FromValue t => FromValue (Maybe t) where
  fromValue = \case
    VNil         -> pure Nothing
    VCell v VNil -> Just <$> fromValue v
    v            -> Left v

instance FromValue t => FromValue [t] where
  fromValue = \case
    VCell t ts -> (:) <$> fromValue t <*> fromValue ts
    VNil       -> pure []
    v          -> Left v

instance (FromValue a, FromValue b) => FromValue (a, b) where
  fromValue = \case
    VCell a b -> (,) <$> fromValue a <*> fromValue b
    v         -> Left v

instance (FromValue a, FromValue b, FromValue c) => FromValue (a, b, c) where
  fromValue (VCell a (VCell b c)) = do
    va <- fromValue a
    vb <- fromValue b
    vc <- fromValue c
    pure (va, vb, vc)
  fromValue v = Left v

fromListy :: FromValue a => ([a] -> b) -> Value -> Either Value b
fromListy f = fmap f . fromValue

fromAfter1Listy :: (FromValue a, FromValue b) => (a -> [b] -> c) -> Value -> Either Value c
fromAfter1Listy c (VCell a b) = c <$> fromValue a <*> fromValue b
fromAfter1Listy c v = Left v

fromAfter2Listy :: (FromValue a, FromValue b, FromValue c)
                => (a -> b -> [c] -> d) -> Value -> Either Value d
fromAfter2Listy c (VCell a bc) = fromValue a >>= \ va -> fromAfter1Listy (c va) bc
fromAfter2Listy c v = Left v

fromTakes1 :: FromValue a => (a -> b) -> Value -> Either Value b
fromTakes1 f (VCell a VNil) = f <$> fromValue a
fromTakes1 f v = Left v

fromTakes2 :: (FromValue a, FromValue b) => (a -> b -> c) -> Value -> Either Value c
fromTakes2 f (VCell a x) = fromValue a >>= \ va -> fromTakes1 (f va) x
fromTakes2 f v = Left v

fromTakes3 :: (FromValue a, FromValue b, FromValue c)
       => (a -> b -> c -> d) -> Value -> Either Value d
fromTakes3 f (VCell a x) = fromValue a >>= \ va -> fromTakes2 (f va) x
fromTakes3 f v = Left v


---------------------------------------------------------------------------
-- THE BACK DOOR
---------------------------------------------------------------------------

-- This turns stuff quoted in LISP into actual values!

lispValue :: LISP -> Value
lispValue NIL        = VNil
lispValue (ATOM a)   = VAtom (MkAtom a)
lispValue (CONS s t) = VCell (lispValue s) (lispValue t)
lispValue (STR t)    = VString "" t
lispValue (RAT r)    = VLit (Num r)
lispValue (BOO b)    = VLit (Boolean b)


---------------------------------------------------------------------------
-- SERIALIZATION
---------------------------------------------------------------------------

instance LISPY v => LISPY (Value' Atom v) where
  toLISP (VAtom a)      = toLISP a
  toLISP (VString _ t)  = STR t
  toLISP VNil           = NIL
  toLISP (VLit l)       = toLISP l
  toLISP (VCell s t)    = "Cell" -: [toLISP s, toLISP t]
  toLISP (VPrim p hss)  = "Prim" -: [ATOM p, toLISP hss]
  toLISP (VFun k rho hss cs) = "Fun" -:
    [toLISP k, toLISP rho, toLISP hss, toLISP cs]
  toLISP (VThunk c)     = "Thunk" -: [toLISP c]
  toLISP (VEnv rho)     = "Env" -: [toLISP rho]
  fromLISP NIL      = pure VNil
  fromLISP (STR t)  = pure (VString "" t)
  fromLISP t = VAtom <$> fromLISP t
           <|> VLit <$> fromLISP t
           <|> (spil t >>= \case
    ("Lit", [l])     -> VLit <$> fromLISP l
    ("Cell", [s, t]) -> VCell <$> fromLISP s <*> fromLISP t
    ("Prim", [ATOM p, hss]) -> VPrim p <$> fromLISP hss
    ("Fun", [k, rho, hss, cs]) -> VFun
      <$> fromLISP k
      <*> fromLISP rho
      <*> fromLISP hss
      <*> fromLISP cs
    ("Thunk", [c]) -> VThunk <$> fromLISP c
    ("Env", [rho]) -> VEnv <$> fromLISP rho
    _ -> Nothing)

instance LISPY v => LISPY (LocalEnv' Atom v) where
  toLISP rho = toLISP $ map (\ (k, v) -> CONS (ATOM k) (toLISP v))
                            (toAscList rho)
  fromLISP (CONS (ATOM x) v) = singleton x <$> fromLISP v
  fromLISP (CONS e1 e2)      = merge <$> fromLISP e2 <*> fromLISP e1
  fromLISP NIL               = pure mempty
  fromLISP _                 = Nothing

instance LISPY v => LISPY (Computation' Atom v) where
  toLISP (Value v)           = toLISP v
  toLISP (Request (a, vs) k) =
    "Request" -: [CONS (toLISP a) (toLISP vs), toLISP k]
  fromLISP t = Value <$> fromLISP t <|> (spil t >>= \case
    ("Request", [(CONS a vs), k]) -> do
      Request <$> ((,) <$> fromLISP a <*> fromLISP vs) <*> fromLISP k
    _ -> Nothing
    )

instance LISPY v => LISPY (Continuation' Atom v) where
  toLISP (Cn a h)     = CONS (toLISP a) (toLISP h)
  fromLISP (CONS a h) = Cn <$> fromLISP a <*> fromLISP h
  fromLISP _          = Nothing

instance LISPY v => LISPY (Frame' Atom v) where
  toLISP (CellL rho t) = "CellL" -: [toLISP rho, toLISP t]
  toLISP (CellR v rho) = "CellR" -: [toLISP v, toLISP rho]
  toLISP (AppL rho ts) = "AppL" -: (toLISP rho : map toLISP ts)
  toLISP (AppR f vz hsrho hsts) = "AppR" -:
    [ toLISP f
    , toLISP vz
    , toLISP hsrho
    , toLISP hsts
    ]
  toLISP (SemiL rho t) = "SemiL" -: [toLISP rho, toLISP t]
  toLISP (PrioL rho t) = "PrioL" -: [toLISP rho, toLISP t]
  toLISP (StringLR v rho ts t) = "StringLR" -:
    [toLISP v, toLISP rho, toLISP ts, toLISP t]
  toLISP (MatchR p) = "MatchR" -: [toLISP p]
  toLISP (Masking a) = "Masking" -: [toLISP a]
  toLISP (Clauses rho cs as) = "Clauses" -: [toLISP rho, toLISP cs, toLISP as]
  fromLISP t = spil t >>= \case
    ("CellL", [rho, t]) -> CellL <$> fromLISP rho <*> fromLISP t
    ("CellR", [v, rho]) -> CellR <$> fromLISP v <*> fromLISP rho
    ("AppL", rho : ts)  -> AppL <$> fromLISP rho <*> traverse fromLISP ts
    ("AppR", [f, cz, hsrho, hsts]) -> AppR
      <$> fromLISP f
      <*> fromLISP cz
      <*> fromLISP hsrho
      <*> fromLISP hsts
    ("SemiL", [rho, t]) -> SemiL <$> fromLISP rho <*> fromLISP t
    ("PrioL", [rho, t]) -> PrioL <$> fromLISP rho <*> fromLISP t
    ("StringLR", [v, rho, ts, t]) -> StringLR
      <$> fromLISP v
      <*> fromLISP rho
      <*> fromLISP ts
      <*> fromLISP t
    ("MatchR", [p]) -> MatchR <$> fromLISP p
    ("Masking", [a]) -> Masking <$> fromLISP a
    ("Clauses", [rho, cs, as]) -> Clauses
      <$> fromLISP rho
      <*> fromLISP cs
      <*> fromLISP as
    _ -> Nothing

instance LISPY v => LISPY (Funy' Atom v) where
  toLISP (FAtom a)     = toLISP a
  toLISP (FPrim p)     = "Prim" -: [ATOM p]
  toLISP (FFun rho cs) = "Fun" -: [toLISP rho, toLISP cs]
  fromLISP t = FAtom <$> fromLISP t <|> (spil t >>= \case
    ("Prim", [ATOM p]) -> pure (FPrim p)
    ("Fun", [rho, cs]) -> FFun <$> fromLISP rho <*> fromLISP cs
    _ -> Nothing)
