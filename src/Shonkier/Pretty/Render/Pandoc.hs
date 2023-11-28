module Shonkier.Pretty.Render.Pandoc where

import Prettyprinter hiding (Doc)
import Prettyprinter.Render.Util.SimpleDocTree
import Text.Pandoc.Builder
import Data.Text (Text)
import qualified Data.Text as T

import Shonkier.Pretty


class FromDoc a where
  render :: Doc -> a

instance FromDoc Inline where
  render = Span ("", ["shonkier-pretty"], []) . render

instance FromDoc Block where
  render = Div ("", ["shonkier-pretty"], []) . pure . Plain . render

instance FromDoc [Inline] where
  render = renderTree . treeForm . layoutPretty defaultLayoutOptions

renderTree :: SimpleDocTree Annotation -> [Inline]
renderTree = \case
  STEmpty           -> mempty
  STChar c          -> pure $ Str (T.singleton c)
  STText _ t        -> pure $ Str t
  STLine i          -> pure LineBreak
  STAnn ann content -> pure $ Span ("", [renderAnn ann], []) $ renderTree content
  STConcat contents -> foldMap renderTree contents

renderAnn :: Annotation -> Text
renderAnn ann = "shonkier-" <> case ann of
  AnnAtom      -> "atom"
  AnnBoolean   -> "boolean"
  AnnError     -> "error"
  AnnFunction  -> "function"
  AnnKeyword   -> "keyword"
  AnnNumeric   -> "numeric"
  AnnOperator  -> "operator"
  AnnPrimitive -> "primitive"
  AnnSplice    -> "splice"
  AnnString    -> "string"
