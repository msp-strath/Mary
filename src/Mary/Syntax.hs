module Mary.Syntax where

import Data.Text (Text)
import Shonkier.Syntax

data ReadWriteStatus
  = ReadOnly
  | WriteOnly
  | ReadWrite

newtype StoreName
  = StoreName { getStoreName :: Text }
  deriving Show
newtype MaryExpr
  = MaryExpr { getMaryExpr :: Text }
  deriving Show
newtype ClassName
  = ClassName { getClassName :: Text }
  deriving Show

data DataDirective = DataDirective
  { storeName :: StoreName
  , dataRWStatus :: ReadWriteStatus
  , dataSource :: RawTerm
  , dataFormat :: Maybe RawTerm
  }
