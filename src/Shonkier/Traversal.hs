module Shonkier.Traversal where

import Control.Monad.Identity

import Shonkier.Syntax
import Shonkier.Value

termA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Term' n a v -> m (Term' n' a' v')
termA' f g h = go where

  go = \case
    Namespace n   -> Namespace <$> f n
    Atom a        -> Atom <$> g a
    Var  v        -> Var <$> h v
    Nil           -> pure Nil
    Lit l         -> pure (Lit l)
    String k es l -> (\ es -> String k es l) <$> traverse (traverse go) es
    Blank         -> pure Blank
    Cell a b      -> Cell <$> go a <*> go b
    App f as      -> App <$> go f <*> traverse go as
    Semi a b      -> Semi <$> go a <*> go b
    Prio a b      -> Prio <$> go a <*> go b
    Fun hs cls    -> Fun <$> traverse (traverse g) hs
                         <*> traverse (clauseA' f g h) cls
    Match p t     -> Match <$> pvalueA' g p <*> go t
    Mask a t      -> Mask <$> g a <*> go t

clauseA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Clause' n a v -> m (Clause' n' a' v')
clauseA' f g h (ps :-> rs) =
  (:->) <$> traverse (pcomputationA' g) ps
        <*> traverse (rhsA' f g h) rs

pcomputationA'
  :: Applicative m =>
     (a -> m a') ->
     PComputation' a -> m (PComputation' a')
pcomputationA' g = go where

  go = \case
    PValue p           -> PValue <$> pvalueA' g p
    PRequest (a, ps) k ->
      PRequest <$> ((,) <$> g a <*> traverse (pvalueA' g) ps)
               <*> pure k
    PThunk k           -> pure (PThunk k)

pvalueA'
  :: Applicative m =>
     (a -> m a') ->
     PValue' a -> m (PValue' a')
pvalueA' g = go where

  go = \case
    PAtom a        -> PAtom <$> g a
    PLit l         -> pure (PLit l)
    PString k es t -> (\ es -> PString k es t) <$> traverse (traverse go) es
    PBind v        -> pure (PBind v)
    PWild          -> pure PWild
    PAs v p        -> PAs v <$> go p
    PNil           -> pure PNil
    PCell a b      -> PCell <$> go a <*> go b

rhsA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Rhs' n a v -> m (Rhs' n' a' v')
rhsA' f g h (gd :?> t) = (:?>) <$> traverse (termA' f g h) gd
                               <*> termA' f g h t

requestA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Request' n a v -> m (Request' n' a' v')
requestA' f g h (a, vs) = (,) <$> g a
                              <*> traverse (valueA' f g h) vs

computationA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Computation' n a v -> m (Computation' n' a' v')
computationA' f g h = go where

  go = \case
    Value v     -> Value <$> valueA' f g h v
    Request r k -> Request <$> requestA' f g h r
                           <*> continuationA' f g h k

valueA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Value' n a v -> m (Value' n' a' v')
valueA' f g h = go where

  go = \case
    VAtom a          -> VAtom <$> g a
    VLit l           -> pure (VLit l)
    VNil             -> pure VNil
    VCell a b        -> VCell <$> go a <*> go b
    VString k str    -> pure (VString k str)
    VPrim a b        -> VPrim a <$> traverse (traverse g) b
    VFun k rho hs cs ->
      VFun <$> continuationA' f g h k
           <*> localEnvA' f g h rho
           <*> traverse (traverse g) hs
           <*> traverse (clauseA' f g h) cs
    VThunk k         -> VThunk <$> computationA' f g h k
    VEnv rho         -> VEnv <$> localEnvA' f g h rho

localEnvA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     LocalEnv' n a v -> m (LocalEnv' n' a' v')
localEnvA' f g h = traverse (valueA' f g h)

funyA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Funy' n a v -> m (Funy' n' a' v')
funyA' f g h = \case
  FAtom a      -> FAtom <$> g a
  FPrim p      -> pure (FPrim p)
  FFun rho cls -> FFun <$> localEnvA' f g h rho
                       <*> traverse (clauseA' f g h) cls

frameA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Frame' n a v -> m (Frame' n' a' v')
frameA' f g h = go where

  go = \case
    CellL rho t -> CellL <$> localEnvA' f g h rho
                         <*> termA' f g h t
    CellR v rho -> CellR <$> valueA' f g h v
                         <*> localEnvA' f g h rho
    AppL rho ts -> AppL <$> localEnvA' f g h rho
                        <*> traverse (termA' f g h) ts
    AppR fn cs (hs, rho) ats ->
      AppR <$> funyA' f g h fn
           <*> traverse (computationA' f g h) cs
           <*> ((,) <$> traverse g hs
                    <*> localEnvA' f g h rho)
           <*> traverse (\ (as, t) -> (,) <$> traverse g as
                                          <*> termA' f g h t)
                        ats
    SemiL rho t -> SemiL <$> localEnvA' f g h rho
                         <*> termA' f g h t
    PrioL rho t -> PrioL <$> localEnvA' f g h rho
                         <*> termA' f g h t
    StringLR v rho sts str ->
      StringLR <$> valueA' f g h v
               <*> localEnvA' f g h rho
               <*> traverse (traverse (termA' f g h)) sts
               <*> pure str
    MatchR ps -> MatchR <$> pvalueA' g ps
    Masking a -> Masking <$> g a
    Clauses rho cls cs ->
      Clauses <$> localEnvA' f g h rho
              <*> traverse (clauseA' f g h) cls
              <*> traverse (computationA' f g h) cs

handListA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     HandList' n a v -> m (HandList' n' a' v')
handListA' f g h = traverse $ \ (hd, frs) ->
  (,) <$> frameA' f g h hd
      <*> traverse (frameA' f g h) frs

continuationA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Continuation' n a v -> m (Continuation' n' a' v')
continuationA' f g h (Cn frs hdls) =
  Cn <$> traverse (frameA' f g h) frs
     <*> handListA' f g h hdls

globalEnvA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     GlobalEnv' n a v -> m (GlobalEnv' n' a' v')
globalEnvA' f g h = traverse (traverse (valueA' f g h))

programA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Program' n a v -> m (Program' n' a' v')
programA' f g h = traverse $ traverse
                $ either (fmap Left . traverse (traverse g))
                         (fmap Right . clauseA' f g h)

moduleA'
  :: Applicative m =>
     (n -> m n') -> (a -> m a') -> (v -> m v') ->
     Module' n a v -> m (Module' n' a' v')
moduleA' f g h = traverse (programA' f g h)

------------------------------------------------------------------------
-- PURE VERSIONS
------------------------------------------------------------------------

globalEnv' ::
  (n -> n') -> (a -> a') -> (v -> v') ->
  GlobalEnv' n a v -> GlobalEnv' n' a' v'
globalEnv' f g h gl = runIdentity $
  globalEnvA' (Identity . f)
              (Identity . g)
              (Identity . h)
              gl

module' ::
  (n -> n') -> (a -> a') -> (v -> v') ->
  Module' n a v -> Module' n' a' v'
module' f g h m = runIdentity $
  moduleA' (Identity . f)
           (Identity . g)
           (Identity . h)
           m
