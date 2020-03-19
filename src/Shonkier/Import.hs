module Shonkier.Import where

import Control.Arrow
import Control.Monad.State

import Data.Either (isRight)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (nub, groupBy, sortBy)
import qualified Data.Map as Map
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO

import System.Directory

import Shonkier.Syntax
import Shonkier.Parser
import Shonkier.Scope
import Shonkier.Value
import Shonkier.Primitives

import Utils.List

data ImportState = ImportState
  { visited :: Set FilePath
  -- ^ Set of visited modules. The import DAG could have a lot
  --   of shared nodes and we only need to load them once.
  , globals :: GlobalEnv
  -- ^ Global environment of accumulated definitions collected
  --   from the visited nodes.
  } deriving (Show)

emptyImportState :: ImportState
emptyImportState = ImportState
  { visited    = Set.empty
  , globals    = primEnv
  }

newtype ImportT m a = ImportT { getImportT :: StateT ImportState m a }
  deriving ( Functor, Applicative, Monad
           , MonadState ImportState, MonadIO)

runImportT :: Monad m
           => ImportT m a -> ImportState -> m (a, ImportState)
runImportT m s = (`runStateT` s) $ getImportT m

evalImportT :: Monad m => ImportT m a -> ImportState -> m a
evalImportT m s = fst <$> runImportT m s

execImportT :: Monad m => ImportT m a -> ImportState -> m ImportState
execImportT m s = snd <$> runImportT m s

forceReadModule :: FilePath -> IO RawModule
forceReadModule fp = getMeAModule <$> TIO.readFile fp

forceImportModule :: FilePath -> ImportT IO Module
forceImportModule fp = do
  rm <- liftIO $ forceReadModule fp
  loadModule fp rm

importModule :: FilePath -> ImportT IO (Maybe Module)
importModule fp = do
  cached <- gets (Set.member fp . visited)
  exists <- liftIO $ doesFileExist fp
  if cached || not exists
    then pure Nothing
    else Just <$> forceImportModule fp

loadModule :: FilePath -> RawModule -> ImportT IO Module
loadModule fp (is, ls) = do
  mapM_ (importModule . fst) is
  scope <- gets (fmap Map.keysSet . globals)
  let ps  = checkRaw fp is scope ls
  let env = mkGlobalEnv fp ps
  modify (\ r -> r { globals = Map.unionWith (<>) (globals r) env
                   , visited = Set.insert fp (visited r)
                   })
  pure (is, ps)

mkGlobalEnv :: FilePath -> Program -> GlobalEnv
mkGlobalEnv fp ls = fold
  [ Map.singleton f $ Map.singleton fp
    $ VFun [] mempty
      (map nub (foldr padCat [] [hs | (_, Left hs) <- grp]))
      [cl | (_, Right cl) <- grp]
  | grp@((f, _) : _) <- groupBy ((==) `on` fst) $
                     -- Note: sortBy is stable
                        sortBy (compare `on` (id *** isRight)) $
                        ls
  ]

loadToplevelModule :: FilePath -> RawModule -> IO (Module, GlobalEnv)
loadToplevelModule fp rm = do
  (m, st) <- runImportT (loadModule fp rm) emptyImportState
  pure (m, globals st)

importToplevelModule :: FilePath -> IO (Module, GlobalEnv)
importToplevelModule fp = do
  rm       <-forceReadModule fp
  (m , st) <- runImportT (loadModule fp rm) emptyImportState
  pure (m, globals st)
