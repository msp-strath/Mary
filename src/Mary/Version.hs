{-# LANGUAGE TemplateHaskell #-}

module Mary.Version where

import Control.Monad (guard)

import Data.Version (showVersion)
import Development.GitRev
import qualified Paths_mary as Mary

version :: String
version = showVersion Mary.version ++ maybe "" ('-':) commit

commit :: Maybe String
commit = do
  let hash = $(gitHash)
  guard (hash /= "UNKNOWN")
  pure $ take 7 hash
