module Mary.Interpreter where


import Control.Monad.Trans (MonadIO)
import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Reader (ReaderT, runReaderT, asks,local)
import Control.Newtype

import Data.Attoparsec.Text
import Data.Foldable
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (First(..))
import Data.Text (Text)
import qualified Data.Text as T

import Network.URI.Encode as URI

import Text.Pandoc.Builder
import Text.Pandoc.Walk

import System.Directory
import System.FilePath


type MaryM = ReaderT MaryCtxt IO

data MaryCtxt = MaryCtxt
  { page :: Text
  , sitesRoot :: Text
  , baseURL :: Text
  , user :: Maybe Text
  , inputs :: Map Text Text
  }

runMaryM :: MaryM x -> IO x
runMaryM = flip runReaderT undefined

class Interpretable a b where
  interpret :: a -> MaryM b
  extract :: b -> a

  default extract :: b ~ a => b -> a
  extract = id

instance Interpretable Pandoc Pandoc where
  interpret (Pandoc meta docs) = do
    (meta, ctx) <- interpret meta
    local (const ctx) $ Pandoc meta <$> interpret docs

instance Interpretable Meta (Meta, MaryCtxt) where
  interpret x = pure (x, undefined)
  extract = fst

instance Interpretable Block Block where
  interpret x = pure x

instance Interpretable a b => Interpretable [a] [b] where
  interpret = traverse interpret
  extract = map extract

process :: Pandoc -> IO Pandoc
process = runMaryM . interpret
