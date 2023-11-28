{-# LANGUAGE OverloadedStrings #-}

module Mary.Interpreter where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State  (StateT, evalStateT, gets, modify)
import Control.Monad.Reader (ReaderT, runReaderT, local, asks)

import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Network.URI.Encode as URI

import Shonkier.Pandoc ()
import Shonkier.Parser (getMeAModule, topTerm)
import Shonkier.Pretty (pretty, toString)
import Shonkier.Pretty.Render.Pandoc (render, FromDoc())
import Shonkier.Syntax (Import, RawModule)
import Shonkier.Semantics (rawShonkier, handleInputs, handleDot)
import Shonkier.Value (Computation, Computation'(..), Env, FromValue(..))

import Text.Pandoc.Builder

-- import System.Directory
-- import System.FilePath

newtype StoreName = StoreName { getStoreName :: Text } deriving Show
newtype MaryExpr = MaryExpr { getMaryExpr :: Text } deriving Show
newtype ClassName = ClassName { getClassName :: Text } deriving Show

-- | Attached to code blocks and code spans
data MaryCodeAttr
  = MaryData
  | MaryDefn
  | MaryEval

isMaryCodeAttr :: Text -> Maybe MaryCodeAttr
isMaryCodeAttr cl
  | cl == "mary-data" = pure MaryData
  | cl == "mary-def" = pure MaryDefn
  | cl `elem` ["mary", "mary-eval"] = pure MaryEval -- TODO: get rid of "mary"
  | otherwise = Nothing

-- | Attached to divs and spans to contextualise their content
data MaryOutAttr
  = MaryApply MaryExpr
  | MaryStore StoreName -- read/write
  | CodeDefault ClassName
  deriving Show

isMaryOutAttr :: (Text, Text) -> Maybe MaryOutAttr
isMaryOutAttr (k, v)
  | k == "mary-apply" = pure (MaryApply (MaryExpr v))
  | k == "mary-store" = pure (MaryStore (StoreName v))
  | k == "code-default" = pure (CodeDefault (ClassName v))
  | otherwise = Nothing

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe p = foldMap $ \ a -> case p a of
  Nothing -> ([a], [])
  Just b -> ([], [b])

instance Interpretable Attr ((Maybe MaryCodeAttr, [MaryOutAttr]), Attr) where
  interpret (id, cls, kvs)
     = let (cls', cattrs) = partitionMaybe isMaryCodeAttr cls in
       let (kvs', oattrs) = partitionMaybe isMaryOutAttr kvs in
       let attr = (id, cls', kvs') in
       case cattrs of
         [] -> pure ((Nothing, oattrs), attr)
         [cattr] -> pure ((Just cattr, oattrs), attr)
         _ : _ : _ -> throwError MoreThanOneCodeAttribute

  extract = snd

data MaryError
  = MoreThanOneCodeAttribute
  | OutAttributesInACodeBlock [MaryOutAttr]
  deriving Show

type MaryM
  = ReaderT MaryCtxt
  ( StateT  MaryState
  ( (ExceptT MaryError IO)))

data MaryDefinition
  = Module RawModule
  | DivTemplate Text Attr [Block]

data MaryState = MaryState
  { definitions :: [MaryDefinition]
  , imports :: [Import]
  }

initMaryState :: MaryState
initMaryState = MaryState
  { definitions = mempty
  , imports = mempty
  }

data MaryCtxt = MaryCtxt
  { filename :: FilePath
  , page :: Text
  , sitesRoot :: Text
  , baseURL :: Text
  , user :: Maybe Text
  , inputs :: Map Text Text
  , environment :: Env
  }

initMaryCtxt :: FilePath -> MaryCtxt
initMaryCtxt fp = MaryCtxt
  { filename = fp
  , page  = def
  , sitesRoot  = def
  , baseURL = def
  , user = def
  , inputs = def
  , environment = mempty
  }
  where
    def = error "INTERNAL ERROR: This should have been overwritten by the interpret instance for Pandoc"

runMaryM :: FilePath -> MaryM x -> IO (Either MaryError x)
runMaryM fp
  = runExceptT
  . flip evalStateT initMaryState
  . flip runReaderT (initMaryCtxt fp)

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
  grab (MetaInlines xs) = T.concat $ map inlineToString xs
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
    filename <- asks filename
    environment <- asks environment
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

isMaryCode :: Attr -> MaryM (Maybe MaryCodeAttr, Attr)
isMaryCode attr = interpret attr >>= \case
  ((mb, []), attr) -> pure (mb, attr)
  ((_, oattrs), _) -> throwError (OutAttributesInACodeBlock oattrs)

-- TODO: are these the best way to do it?
nullBlock :: Block
nullBlock = Plain []

nullInline :: Inline
nullInline = Str ""

instance Interpretable Block Block where
  interpret = \case
    CodeBlock attr txt -> isMaryCode attr >>= \case
      (Just cb, attr@(_, cls, _)) -> case cb of
        MaryData -> undefined
        MaryDefn -> fromMaybe nullBlock <$> defnMary cls txt
        MaryEval -> evalMary txt
      (Nothing, attr) -> pure (CodeBlock attr txt)
    Div attr bs -> undefined
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

defnMary :: FromDoc b => [Text] -> Text -> MaryM (Maybe b)
defnMary cls txt = do
  let mod = getMeAModule txt
  modify (\ st -> st { definitions = Module mod : definitions st })
  -- TODO: distinguish raw keep vs. syntax highlighting?
  pure $ do
    guard ("keep" `elem` cls)
    pure (render (pretty mod))


evalMary :: FromValue b => Text -> MaryM b
evalMary e =
  case parseOnly (topTerm <* endOfInput) e of
    Left err -> error err
    Right t -> do
      is <- gets imports
      fp <- asks filename
      env@(gl,_) <- asks environment
      -- TODO: we need to strip off the common var prefix from our term
      -- lcp <- readPrefixToStrip
      -- let t' = fmap (stripVarPrefix lcp) t
      let t' = t
      go env (rawShonkier is fp gl t')
  where
  go :: FromValue b => Env -> Computation -> MaryM b
  go _ (Value v) = case fromValue v of
                     Right p  -> pure p
                     Left foc -> error $ unlines
                       [ "Invalid value: " ++ show foc
                       , "in result:"
                       , toString v
                       ]
  go gamma (Request r@(a, vs) k)
    | a `elem` ["POST", "GET", "meta"] = handleInputs (go gamma) gamma r k
    | a `elem` ["dot"]                 = handleDot (go gamma) gamma r k
  go _ r@Request{} = error (show r)

  -- stripVarPrefix :: String -> RawVariable -> RawVariable
  -- stripVarPrefix lcp (p :.: x) = (stripPrefixButDot lcp <$> p) :.: x


instance Interpretable Inline Inline where
  interpret = \case
    Code attr txt -> isMaryCode attr >>= \case
      (Just cb, attr@(_, cls, _)) -> case cb of
        MaryData -> undefined
        MaryDefn -> fromMaybe nullInline <$> defnMary cls txt
        MaryEval -> evalMary txt
      (Nothing, attr) -> pure (Code attr txt)
    Span attr is -> undefined
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

process :: FilePath -> Pandoc -> IO Pandoc
process fp pdoc = do
  pdoc' <- runMaryM fp (interpret pdoc)
  case pdoc' of
    Left err -> error (show err)
    Right pdoc -> pure pdoc
