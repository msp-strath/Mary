module Shonkier.Import where

import Control.Arrow
import Control.Monad.State

import Data.Either (isRight)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (nub, groupBy, sortBy, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.IO as TIO

import System.Directory
import System.FilePath

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

forceImportModule :: [FilePath] -> FilePath -> ImportT IO Module
forceImportModule base fp = do
  let file = joinPath base </> fp
  rm <- liftIO $ forceReadModule file
  loadModule base file rm

-- Returns the path where the module was found, and maybe the module
-- itself if we haven't visited it already.
importModule :: [FilePath] -> FilePath -> ImportT IO (FilePath, Maybe Module)
importModule base fp = do
  let file = joinPath base </> fp
  cached <- gets (Set.member file . visited)
  if cached
    -- we've seen it before!
    then pure (file, Nothing)
  else do
    exists <- liftIO $ doesFileExist file
    if exists
      -- we found it!
      then do
        m <- forceImportModule base file
        pure (file, Just m)
      else case base of
        -- we've hit (filesystem) rock bottom
        [] -> error $ "import does not exist: " ++ fp
        -- otherwise try again moving closer to the root
        _  -> importModule (init base) fp

-- The filepath @fp@ is the import asked for, so joining it to base we
-- get an absolute path to our current attempt at importing.
-- File references returned are absolute.
loadModule :: [FilePath] -> FilePath -> RawModule -> ImportT IO Module
loadModule base fp (is, ls) = do
  let file = joinPath base </> fp
  is' <- mapM updateModule is
  scope <- gets (fmap Map.keysSet . globals)
  let ps  = checkRaw file is' scope ls
  let env = mkGlobalEnv file ps
  modify (\ r -> r { globals = Map.unionWith (<>) (globals r) env
                   , visited = Set.insert file (visited r)
                   })
  pure (is, ps)
  where
    updateModule (i, mn) = do
        (i', _) <- importModule base i
        pure (i' , mn)

-- Apart from loading the top level module, also simplifies the
-- returned module and global env by stripping out the longest common
-- prefix from all paths therein. Hence if all imported files are in
-- the directory /home/biffo/shonkier-projects/, this prefix will be
-- stripped out in the interest of read- and port-ability.
loadToplevelModule :: FilePath -> RawModule -> IO (Module, GlobalEnv)
loadToplevelModule fp rm = do
  base <- takeDirectory <$> makeAbsolute fp
  (m, st) <- runImportT (loadModule (splitPath base) (takeFileName fp) rm) emptyImportState
  -- strip common prefix from global env
  let (lcp, gl) = simplifyGlobalEnv $ globals st
  -- strip common prefix from all programs
  let m' = (second (fmap (fmap (fmap (fmap $ simplifyTerm lcp))))) m
  pure (m', gl)

importToplevelModule :: FilePath -> IO (Module, GlobalEnv)
importToplevelModule fp = forceReadModule fp >>= loadToplevelModule fp

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

-- Returns the longest common prefix of all filepaths occurring in its
-- argument, together with a simplified global env where this prefix
-- has been stripped everywhere
simplifyGlobalEnv :: GlobalEnv -> (String, GlobalEnv)
simplifyGlobalEnv gl = (lcp, gl') where
  lcp = longestCommonPrefix $ filter (/= ".")
          $ concatMap Map.keys $ Map.elems gl
  gl' = Map.map (Map.map (simplifyTerm lcp))
          $ Map.map (Map.mapKeys $ stripPrefixButDot lcp) gl

simplifyTerm :: Functor f => String -> f ScopedVariable -> f ScopedVariable
simplifyTerm lcp = fmap simp
  where
    simp :: ScopedVariable -> ScopedVariable
    simp (GlobalVar fp x) = GlobalVar (stripPrefixButDot lcp fp) x
    simp y                = y

stripPrefixButDot :: String -> String -> String
stripPrefixButDot prf "." = "."
stripPrefixButDot prf x   =
  let stripped = fromJust $ stripPrefix prf x in
  if null stripped then "." else stripped
