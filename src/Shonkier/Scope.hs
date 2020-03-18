{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}

module Shonkier.Scope
       ( GlobalScope
       , checkProgram
       ) where

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

data ScopeState = ScopeState
  { currentFile :: FilePath
  , imports     :: Set FilePath
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

checkProgram :: FilePath -> Set FilePath -> GlobalScope -> RawProgram -> Program
checkProgram fp is gl p = evalScopeM (scopeCheck Set.empty p) (ScopeState fp is gl)


instance ScopeCheck RawProgram Program where
  scopeCheck local ps = do
    unless (Set.null local) $
      error $ "*** Error: Local environment should be empty"
            ++ "when scope-checking a program!"
    mapM_ (declare . fst) ps
    forM ps $ \case
      (nm, Left decl) -> pure (nm, Left decl)
      (nm, Right cl)  -> (nm,) . Right <$> scopeCheck local cl

instance ScopeCheck RawTerm Term where
  scopeCheck local = \case
    Atom a -> pure (Atom a)
    Lit l  -> pure (Lit l)
    Var v | Set.member v local -> pure (Var $ LocalVar v)
          | otherwise -> get >>= \ st -> pure $ Var $
              case Set.toList . Set.intersection (imports st) <$> globalScope st Map.!? v of
                Just [fp]      -> GlobalVar fp v
                Just fps@(_:_) -> AmbiguousVar fps v
                -- for some strange reason we seem to be able to generate
                -- a (Just [])!...
                _              -> OutOfScope v
    Cell a b  -> Cell <$> scopeCheck local a <*> scopeCheck local b
    App f ts  -> App <$> scopeCheck local f <*> mapM (scopeCheck local) ts
    Fun hs cs -> Fun hs <$> mapM (scopeCheck local) cs

instance ScopeCheck RawClause Clause where
  scopeCheck local (ps, t) = do
    locals <- mapM (scopeCheck local) ps
    let new = fold locals
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
