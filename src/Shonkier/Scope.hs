{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE FlexibleInstances          #-}

module Shonkier.Scope
     {-  ( GlobalScope
       , checkRaw
       ) -} where

import Control.Monad
import Control.Monad.State

import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2

import Shonkier.Syntax

type Namespaces = Map Namespace (Set FilePath)

declareNamespaces :: [Import] -> Namespaces
declareNamespaces = foldr cons Map.empty where
  cons (fp, mns) r = case mns of
    Nothing -> r
    Just nm -> Map.insertWith Set.union nm (Set.singleton fp) r

data ScopeState = ScopeState
  { currentFile :: FilePath
  , imports     :: Set FilePath
  , namespaces  :: Namespaces
  , globalScope :: GlobalScope
  }

type GlobalScope = Map Variable (Set FilePath)
type LocalScope  = Set Variable

newtype ScopeM a = ScopeM { getScopeM :: State ScopeState a }
  deriving ( Functor, Applicative, Monad
           , MonadState ScopeState)

class ScopeCheck t u | t -> u where
  scopeCheck :: LocalScope -> t -> ScopeM u

declare :: Variable -> ScopeM ()
declare v = do
  fp <- gets currentFile
  let addFP = Just . Set.insert fp . fromMaybe Set.empty
  modify $ \ r -> r { globalScope = Map.alter addFP v (globalScope r) }

evalScopeM :: ScopeM a -> ScopeState -> a
evalScopeM = evalState . getScopeM

checkRaw :: ScopeCheck t u
         => FilePath -> [Import] -> GlobalScope -> t -> u
checkRaw fp is gl p =
  let imported  = Set.fromList $ [".", fp] ++ map fst is
      nmspaces  = declareNamespaces is
      initState = ScopeState fp imported nmspaces gl
  in evalScopeM (scopeCheck Set.empty p) initState

instance ScopeCheck RawProgram Program where
  scopeCheck local ps = do
    unless (Set.null local) $
      error $ "*** Error: Local environment should be empty"
            ++ "when scope-checking a program!"
    mapM_ (declare . fst) ps
    forM ps $ \case
      (nm, Left decl) -> pure (nm, Left decl)
      (nm, Right cl)  -> (nm,) . Right <$> scopeCheck local cl

instance ScopeCheck RawVariable ScopedVariable where
  scopeCheck local (mns, v) = case mns of
    Nothing | Set.member v local -> pure $ LocalVar v
    Just nm -> get >>= \ st -> case namespaces st Map.!? nm of
      Nothing  -> pure $ InvalidNamespace nm v
      Just fps -> checkGlobal fps v
    _ -> get >>= \ st -> checkGlobal (imports st) v

    where

    checkGlobal :: Set FilePath -> Variable -> ScopeM ScopedVariable
    checkGlobal scp v = do
      candidates <- gets (\ st -> globalScope st Map.!? v)
      pure $ case Set.toList . Set.intersection scp <$> candidates of
        Just [fp]      -> GlobalVar fp v
        Just fps@(_:_) -> AmbiguousVar fps v
        _              -> OutOfScope v

instance ScopeCheck RawTerm Term where
  scopeCheck local = \case
    Atom a -> pure (Atom a)
    Lit l  -> pure (Lit l)
    Var v  -> Var <$> scopeCheck local v
    Cell a b  -> Cell <$> scopeCheck local a <*> scopeCheck local b
    App f ts  -> App <$> scopeCheck local f <*> mapM (scopeCheck local) ts
    Semi l r  -> Semi <$> scopeCheck local l <*> scopeCheck local r
    Fun hs cs -> Fun hs <$> traverse (scopeCheck local) cs
    String k sts u ->
      String k
        <$> traverse (traverse (scopeCheck local)) sts
        <*> pure u
    Match p t -> Match p <$> scopeCheck local t  -- for now

instance ScopeCheck RawClause Clause where
  scopeCheck local (ps, t) = do
    locals <- mapM (scopeCheck local) ps
    let new = fold (local : locals)
    (ps,) <$> scopeCheck new t

instance ScopeCheck PComputation LocalScope where
  scopeCheck local (PValue v) = scopeCheck local v
  scopeCheck local (PRequest (a, vs) mk) = do
    local1 <- fold <$> mapM (scopeCheck local) vs
    pure $ maybe id Set.insert mk local1
  scopeCheck local (PThunk k) = pure $ Set.singleton k

instance ScopeCheck PValue LocalScope where
  scopeCheck local = \case
    PAtom{}   -> pure Set.empty
    PLit{}    -> pure Set.empty
    PBind x   -> pure (Set.singleton x)
    PWild{}   -> pure Set.empty
    PAs x p   -> Set.insert x <$> scopeCheck local p
    PCell p q -> (<>) <$> scopeCheck local p <*> scopeCheck local q
    PString _ sps _ ->
      foldMap snd <$> traverse (traverse (scopeCheck local)) sps