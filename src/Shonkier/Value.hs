module Shonkier.Value where

import Control.Monad.State
import Control.Monad.Reader

import Data.Map (Map, singleton, toAscList)
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text (Text)

import Data.Bwd
import Shonkier.Syntax
import Utils.List


---------------------------------------------------------------------------
-- ABORT
---------------------------------------------------------------------------

-- how to signal a failure

abortA :: String
abortA = "abort"


---------------------------------------------------------------------------
-- RHS TRANSLATION
---------------------------------------------------------------------------

rhs2Term :: [Rhs' String v] -> Term' String v
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
type GlobalEnv = GlobalEnv' String ScopedVariable

type LocalEnv' a v = Map Variable (Value' a v)
type LocalEnv = LocalEnv' String ScopedVariable

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
  deriving (Show, Functor)

type Value = Value' String ScopedVariable

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
valueEqHuh x y | hoValue x || hoValue y = Nothing
valueEqHuh _ _ = Just False


---------------------------------------------------------------------------
-- EXPLICIT ENVIRONMENTS
---------------------------------------------------------------------------

value2Env :: Value -> LocalEnv
value2Env (VCell (VAtom x) v) = singleton x v
value2Env (VCell e1 e2)       = merge (value2Env e2) (value2Env e1)
value2Env _                   = mempty

env2value :: LocalEnv -> Value
env2value = foldr (VCell . sing) VNil . toAscList where
  sing (k, v) = VCell (VAtom k) v




---------------------------------------------------------------------------
-- COMPUTATIONS
---------------------------------------------------------------------------

type Request' a v = (a, [Value' a v])
type Request = Request' String ScopedVariable

data Computation' a v
  = Value (Value' a v)
  | Request (Request' a v) (Continuation' a v) -- [Frame' a v]
  -- ^ Invoking an effect & none of the
  -- frames present know how to interpret it
  deriving (Show, Functor)

type Computation = Computation' String ScopedVariable

type HandList' a v =
  [ ( Frame' a v        -- handleFrame
    , Bwd (Frame' a v)  -- all (not . handleFrame)
    )
  ]

type HandList = HandList' String ScopedVariable

data Continuation' a v = Cn
  { aftard   :: Bwd (Frame' a v)   -- all (not . handleFrame)
  , handlers :: HandList' a v
  }
  deriving (Show, Functor)

type Continuation = Continuation' String ScopedVariable

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
type Funy = Funy' String ScopedVariable

-- The argument of type (LocalEnv' a) indicates the
-- cursor position
data Frame' a v
  = CellL (LocalEnv' a v) (Term' a v)
  | CellR (Value' a v) (LocalEnv' a v)
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

type Frame = Frame' String ScopedVariable

data Context' a v = Cx
  { handlerz :: Bwd ( Bwd (Frame' a v)  -- all (not . handleFrame)
                    , Frame' a v        -- handleFrame
                    )
  , nandlerz :: Bwd (Frame' a v)        -- all (not . handleFrame)
  }
type Context = Context' String ScopedVariable

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
  { getShonkier :: StateT Context (Reader Env) a }
  deriving ( Functor, Applicative, Monad
           , MonadState Context, MonadReader Env
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
