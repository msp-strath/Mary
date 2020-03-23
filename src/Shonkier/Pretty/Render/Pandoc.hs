module Shonkier.Pretty.Render.Pandoc where

import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text.Prettyprint.Doc hiding (Doc)
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Text.Pandoc.Builder
import Data.Text (Text)
import qualified Data.Text as T

import Shonkier.Pretty

render :: Doc -> Block
render = Div ("", ["shonkier-pretty"], []) . pure . Plain
       . renderTree . treeForm . layoutPretty defaultLayoutOptions

renderTree :: SimpleDocTree Annotation -> [Inline]
renderTree = \case
  STEmpty           -> mempty
  STChar c          -> pure $ Str (T.singleton c)
  STText _ t        -> pure $ Str t
  STLine i          -> pure $ LineBreak
  STAnn ann content -> pure $ Span ("", [renderAnn ann], []) $ renderTree content
  STConcat contents -> foldMap renderTree contents

renderAnn :: Annotation -> Text
renderAnn ann = "shonkier-" <> case ann of
  AnnAtom      -> "atom"
  AnnError     -> "error"
  AnnFunction  -> "function"
  AnnKeyword   -> "keyword"
  AnnNumeric   -> "numeric"
  AnnPrimitive -> "primitive"
  AnnString    -> "string"


