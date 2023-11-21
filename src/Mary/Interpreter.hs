{-# LANGUAGE OverloadedStrings #-}

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

getGET :: Map Text Text -> Text -> Maybe Text
getGET inputs x = M.lookup ("GET_" <> x) inputs

getPOST :: Map Text Text -> Text -> Maybe Text
getPOST inputs x = M.lookup ("POST_" <> x) inputs

metaToInputValues :: Meta -> Map Text Text
metaToInputValues (Meta m) = M.map grab m where
  grab (MetaInlines xs) = T.concat $ L.map inlineToString xs
  grab x = error $
    "IMPOSSIBLE non-string meta value " ++ show x

  inlineToString = \case
    RawInline (Format "html") s -> URI.decodeText s
    Str s -> s
    x -> error $ "IMPOSSIBLE non-string inline " ++ show x

instance Interpretable Meta (Meta, MaryCtxt) where
  interpret meta = do
    let errorOnFail f x =
         let msg = "Meta data '" <> x <> "' missing!" in
         fromMaybe (error (T.unpack msg)) (f x)
    let inputs = metaToInputValues meta
    -- we assume that certain metadata exists, put there by mary find
    let page = errorOnFail (getGET inputs) "page"
    let sitesRoot = errorOnFail (`M.lookup` inputs) "sitesRoot"
    let baseURL = errorOnFail (`M.lookup` inputs) "baseURL"
    let user = M.lookup "user" inputs
    pure (meta, MaryCtxt {..})

  extract = fst

instance Interpretable Caption Caption where
  interpret = pure -- TODO

instance Interpretable TableHead TableHead where
  interpret = pure -- TODO

instance Interpretable TableBody TableBody where
  interpret = pure -- TODO

instance Interpretable TableFoot TableFoot where
  interpret = pure -- TODO

instance Interpretable Block Block where
  interpret = \case
    CodeBlock attr txt -> _
    Div attr bs -> _
    -- structural
    Plain is -> Plain <$> interpret is
    Para is -> Para <$> interpret is
    LineBlock iss -> LineBlock <$> interpret iss
    BlockQuote bs -> BlockQuote <$> interpret bs
    OrderedList lattr bss -> OrderedList lattr <$> interpret bss
    BulletList bss -> BulletList <$> interpret bss
    DefinitionList ibsss -> DefinitionList <$> interpret ibsss
    Header i attr is -> Header i attr <$> interpret is
    Table attr capt cols th tds tf ->
      Table attr <$> interpret capt
                 <*> pure cols
                 <*> interpret th
                 <*> interpret tds
                 <*> interpret tf
    -- pure
    RawBlock fmt txt -> pure (RawBlock fmt txt)
    HorizontalRule -> pure HorizontalRule

instance Interpretable Inline Inline where
  interpret = \case
    Code attr txt -> _
    Span attr is -> _
    -- structural
    Emph is -> Emph <$> interpret is
    Underline is -> Underline <$> interpret is
    Strong is -> Strong <$> interpret is
    Strikeout is -> Strikeout <$> interpret is
    Superscript is -> Superscript <$> interpret is
    Subscript is -> Subscript <$> interpret is
    SmallCaps is -> SmallCaps <$> interpret is
    Quoted qt is -> Quoted qt <$> interpret is
    Cite ct is -> Cite ct <$> interpret is
    Link attr is tgt -> Link attr <$> interpret is <*> pure tgt
    Image attr is tgt -> Image attr <$> interpret is <*> pure tgt
    Note bs -> Note <$> interpret bs
    -- pure
    Str txt -> pure (Str txt)
    Space -> pure Space
    SoftBreak -> pure SoftBreak
    LineBreak -> pure LineBreak
    Math mty txt -> pure (Math mty txt)
    RawInline fmt txt -> pure (RawInline fmt txt)


instance
  ( Interpretable a c
  , Interpretable b d
  ) => Interpretable (a, b) (c, d) where
  interpret (a, b) = (,) <$> interpret a <*> interpret b
  extract (c, d) = (extract c, extract d)

instance Interpretable a b => Interpretable [a] [b] where
  interpret = traverse interpret
  extract = map extract

process :: Pandoc -> IO Pandoc
process = runMaryM . interpret
