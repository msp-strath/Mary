module Shonkier.Pretty.Examples where

{-
import Shonkier.Examples
import Shonkier.Pretty ()

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

test :: IO ()
test = TIO.putStr
     $ T.unlines
     $ fmap (renderStrict . layoutSmart defaultLayoutOptions . pretty)
     [ appendTest
     , askTest
     , stateTest
     ]
-}
