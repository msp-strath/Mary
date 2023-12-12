{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Mary.Interpreter where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State  (StateT, runStateT, gets)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Newtype (ala')

import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (nub, nubBy)
import Data.Monoid (First(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T

import Network.URI.Encode as URI

import Shonkier.Import (loadToplevelModule, stripPrefixButDot, stripVarPrefix)
import Shonkier.ShonkierJS (jsGlobalEnv, jsInputs)
import Shonkier.Pandoc ()
import Shonkier.Parser (getMeAModule, topTerm, identifier, argTuple, pcomputation, skipSpace)
import Shonkier.Pretty (pretty, toString)
import Shonkier.Pretty.Render.Pandoc (render, FromDoc)
import Shonkier.Syntax (Import, RawModule, Clause'((:->)), Rhs'((:?>)), toRawTerm)
import Shonkier.Semantics (rawShonkier, handleInputs, handleDot)
import Shonkier.Value (Computation, Computation'(..), Env, FromValue(..))

import Text.Pandoc.Builder
import Text.Pandoc.Walk (query)

import System.Directory (makeAbsolute)
import System.FilePath ((</>), joinPath)

newtype StoreName = StoreName { getStoreName :: Text } deriving Show
newtype MaryExpr = MaryExpr { getMaryExpr :: Text } deriving Show
newtype ClassName = ClassName { getClassName :: Text } deriving Show

-- | Attached to code blocks and code spans
data MaryCodeAttr
  = MaryData
  | MaryDefn
  | MaryEval
  | FormInput
  deriving Show

isMaryCodeAttr :: Text -> Maybe MaryCodeAttr
isMaryCodeAttr cl
  | cl == "mary-data" = pure MaryData
  | cl == "mary-def" = pure MaryDefn
  | cl == "mary-eval" = pure MaryEval
  | cl == "input" = pure FormInput
  | otherwise = Nothing

-- | Attached to divs and spans to contextualise their content
data MaryDivAttr
  = MaryApply MaryExpr
  | MaryStore StoreName -- read/write
  | CodeDefault ClassName
  | MaryTemplate MaryExpr
  deriving Show


-- Returns a list without the template
hasMaryTemplate :: [MaryDivAttr] -> Maybe (MaryExpr, [MaryDivAttr])
hasMaryTemplate [] = Nothing
hasMaryTemplate (MaryTemplate v : ds) = pure (v, ds)
hasMaryTemplate (d : ds) = fmap (d :) <$> hasMaryTemplate ds

isMaryDivAttr :: (Text, Text) -> Maybe MaryDivAttr
isMaryDivAttr (k, v)
  | k == "mary-apply" = pure (MaryApply (MaryExpr v))
  | k == "mary-store" = pure (MaryStore (StoreName v))
  | k == "code-default" = pure (CodeDefault (ClassName v))
  | k == "mary-template" = pure (MaryTemplate (MaryExpr v))
  | otherwise = Nothing

unIsMaryDivAttr :: MaryDivAttr -> (Text, Text)
unIsMaryDivAttr = \case
  MaryApply (MaryExpr v) -> ("mary-apply", v)
  MaryStore (StoreName v) -> ("mary-store", v)
  CodeDefault (ClassName v) -> ("code-default", v)
  MaryTemplate (MaryExpr v) -> ("mary-template", v)

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe p = foldMap $ \ a -> case p a of
  Nothing -> ([a], [])
  Just b -> ([], [b])

data MaryError
  = MoreThanOneCodeAttribute
  | DivAttributesInACodeBlock [MaryDivAttr]
  | CodeAttributeInADivBlock MaryCodeAttr
  deriving Show

type MaryM
  = ReaderT MaryCtxt
  ( StateT  MaryState
  ( ExceptT MaryError IO))

data MaryWriter = MaryWriter
  { collFilename :: FilePath
  , collPage :: Text
  , collSitesRoot :: Text
  , collBaseURL :: Text
  , collUser :: Maybe Text
  , collInputs :: Map Text Text
  }

type MaryCollectM
  = ReaderT DefaultCodeAttr
  ( WriterT ([MaryDefinition] -> [MaryDefinition], First MaryWriter)
  ( ExceptT MaryError IO))

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

type DefaultCodeAttr = (Maybe MaryCodeAttr, ([Text], [(Text, Text)]))

fromDefaultCodeAttr :: Phase m -> Attr -> m (Maybe MaryCodeAttr, Attr)
fromDefaultCodeAttr ph (id0, cls0, kvs0) = case ph of
  CollPhase -> asks cast
  EvalPhase -> asks (cast . defaultCodeAttr)

  where
    cast :: DefaultCodeAttr -> (Maybe MaryCodeAttr, Attr)
    cast (mb, (cls1, kvs1))
      = ( mb
        , ( id0
          , nub (cls0 <> cls1)
          , nubBy ((==) `on` fst) (kvs0 <> kvs1)))

data MaryCtxt = MaryCtxt
  { commonPrefix :: String
  , filename :: FilePath
  , page :: Text
  , sitesRoot :: Text
  , baseURL :: Text
  , user :: Maybe Text
  , inputs :: Map Text Text
  , environment :: Env
  , defaultCodeAttr :: DefaultCodeAttr
  }

runMaryM :: MaryCtxt -> MaryM x -> IO (Either MaryError (x, MaryState))
runMaryM ctx
  = runExceptT
  . flip runStateT initMaryState
  . flip runReaderT ctx

runMaryCollectM ::
  MaryCollectM x ->
  IO (Either MaryError (x, ([MaryDefinition], MaryWriter)))
runMaryCollectM
  = runExceptT
  . fmap (fmap $ \case
             (defs, First Nothing)  -> error "The IMPOSSIBLE has happened"
             (defs, First (Just w)) -> (defs [], w))
  . runWriterT
  . flip runReaderT (Nothing, mempty)

data Phase m where
  CollPhase :: Phase MaryCollectM
  EvalPhase :: Phase MaryM

class Interpretable a b where
  interpret :: Monad m => Phase m -> a -> m b
  extract :: b -> a

  default extract :: b ~ a => b -> a
  extract = id

instance Interpretable Attr ((Maybe MaryCodeAttr, [MaryDivAttr]), Attr) where
  interpret ph (id, cls, kvs)
     = let (cls', cattrs) = partitionMaybe isMaryCodeAttr cls in
       let (kvs', oattrs) = partitionMaybe isMaryDivAttr kvs in
       let attr = (id, cls', kvs') in
       case cattrs of
         [] -> pure ((Nothing, oattrs), attr)
         [cattr] -> pure ((Just cattr, oattrs), attr)
         _ : _ : _ -> throwMaryError ph MoreThanOneCodeAttribute

  extract = snd

instance Interpretable Pandoc Pandoc where
  interpret ph (Pandoc meta docs) = do
    meta <- interpret ph meta
    Pandoc meta <$> interpret ph docs

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

instance Interpretable Meta Meta where
  interpret EvalPhase meta = pure meta
  interpret CollPhase meta = do
    let errorOnFail f x =
         let msg = "Meta data '" <> x <> "' missing!" in
         fromMaybe (error (T.unpack msg)) (f x)
    let inputs = metaToInputValues meta
    -- we assume that certain metadata exists, put there by mary find
    let page = errorOnFail (getGET inputs) "page"
    let sitesRoot = errorOnFail (`M.lookup` inputs) "sitesRoot"
    let baseURL = errorOnFail (`M.lookup` inputs) "baseURL"
    filename <- liftIO $ makeAbsolute (T.unpack sitesRoot </> T.unpack page)
    let user = M.lookup "user" inputs
    tell (id, First $ Just $ MaryWriter
      { collFilename = filename
      , collInputs = inputs
      , collPage = page
      , collSitesRoot = sitesRoot
      , collBaseURL = baseURL
      , collUser = user })
    pure meta

instance Interpretable Caption Caption where
  interpret ph (Caption ms bs) = Caption <$> interpret ph ms <*> interpret ph bs

instance Interpretable Cell Cell where
  interpret ph (Cell attr alg rsp csp bs)
    = Cell attr alg rsp csp <$> interpret ph bs

instance Interpretable Row Row where
  interpret ph (Row attr cs) = Row attr <$> interpret ph cs

instance Interpretable TableHead TableHead where
  interpret ph (TableHead attr rs) = TableHead attr <$> interpret ph rs

instance Interpretable TableBody TableBody where
  interpret ph (TableBody attr rhcl rs0 rs1)
    = TableBody attr rhcl <$> interpret ph rs0 <*> interpret ph rs1

instance Interpretable TableFoot TableFoot where
  interpret ph (TableFoot attr rs) = TableFoot attr <$> interpret ph rs

{-# INLINE throwMaryError #-}
throwMaryError :: Phase m -> MaryError -> m a
throwMaryError CollPhase = throwError
throwMaryError EvalPhase = throwError

-- NB: also used for spans
isMaryDiv  :: Monad m => Phase m -> Attr -> m ([MaryDivAttr], Attr)
isMaryDiv ph attr = do
  -- /!\ This interpret better be pure (apart from raising errors)!
  interpret ph attr >>= \case
    ((Nothing, ds), attr) -> pure (ds, attr)
    ((Just b, _), attr) -> throwMaryError ph (CodeAttributeInADivBlock b)

unIsMaryDiv :: [MaryDivAttr] -> Attr -> Attr
unIsMaryDiv ds (id, cls, kvs) = (id, cls, map unIsMaryDivAttr ds ++ kvs) where


isMaryCode :: Monad m => Phase m -> Attr -> m (Maybe MaryCodeAttr, Attr)
isMaryCode ph attr = do
  -- /!\ This interpret better be pure (apart from raising errors)!
  interpret ph attr >>= \case
    ((Nothing, []), attr) -> fromDefaultCodeAttr ph attr
    ((mb, []), attr) -> pure (mb, attr)
    ((_, oattrs), _) -> throwMaryError ph (DivAttributesInACodeBlock oattrs)

input_ :: [Text] -> Maybe Text -> Text
input_ args mval
  = T.intercalate " " ("<input" : args)
  <> maybe "" (\ val -> T.concat [" value=\"", val, "\""]) mval
  <> ">"

textarea_ :: [Text] -> Maybe Text -> Text
textarea_ args mval
  = T.intercalate " " ("<textarea" : args) <> ">"
  <> fromMaybe "" mval <> "</textarea>"

-- Only make forms in the Eval phase
makeInputForm :: Bool -> Attr -> Text -> MaryM Text
makeInputForm _ (_, _, as) p | ("type", "submit") `elem` as
  = pure $ input_ [ T.concat [k, "=\"", v, "\""] | (k, v) <- ("value", p):as ] Nothing
makeInputForm textarea a@(i, cs, as) p = do
  inputs <- asks inputs
  let nameparser = skipSpace *> identifier <* skipSpace
  pure $ case parseOnly nameparser p of
    Left _ -> ""
    Right n -> let name = T.pack n
                   mval = getPOST inputs name in
      (if textarea then textarea_ else input_)
      [ T.concat [k, "=\"", v, "\""] | (k, v) <- ("name",name):("id", name):as ]
      mval

-- TODO: are these the best way to do it?
nullBlock :: Block
nullBlock = Plain []

nullInline :: Inline
nullInline = Str ""

instance Interpretable Block Block where
  interpret ph = \case
    i@(CodeBlock attr txt) -> isMaryCode ph attr >>= \case
      (Just cb, attr@(_, cls, kvs)) -> case cb of
        MaryData -> pure i -- TODO
        MaryDefn -> case ph of
          CollPhase -> do (_ :: Maybe Block) <- defnMary ph cls txt
                          pure i
          EvalPhase -> fromMaybe nullBlock <$> defnMary ph cls txt
        MaryEval -> case ph of
          CollPhase -> pure i
          EvalPhase -> evalMary txt
        FormInput -> case ph of
          CollPhase -> pure i
          EvalPhase ->
            let textarea = "type" `notElem` map fst kvs in
            RawBlock (Format "html") <$> makeInputForm textarea attr txt
      (Nothing, attr) -> pure (CodeBlock attr txt)
    i@(Div attr bs) -> isMaryDiv ph attr >>= \case
      (ds, attr)
        | Just (decl, ds) <- hasMaryTemplate ds ->
          case ph of
            CollPhase ->
              i <$ tell ( (DivTemplate (getMaryExpr decl) (unIsMaryDiv ds attr) bs :)
                        , First Nothing)
            EvalPhase -> pure nullBlock
      _ -> Div attr <$> interpret ph bs
    -- structural
    Plain is -> Plain <$> interpret ph is
    Para is -> Para <$> interpret ph is
    LineBlock iss -> LineBlock <$> interpret ph iss
    BlockQuote bs -> BlockQuote <$> interpret ph bs
    OrderedList lattr bss -> OrderedList lattr <$> interpret ph bss
    BulletList bss -> BulletList <$> interpret ph bss
    DefinitionList ibsss -> DefinitionList <$> interpret ph ibsss
    Header i attr is -> Header i attr <$> interpret ph is
    Table attr capt cols th tds tf ->
      Table attr <$> interpret ph capt
                 <*> pure cols
                 <*> interpret ph th
                 <*> interpret ph tds
                 <*> interpret ph tf
    Figure attr cap bs -> Figure attr <$> interpret ph cap <*> interpret ph bs
    -- pure
    RawBlock fmt txt -> pure (RawBlock fmt txt)
    HorizontalRule -> pure HorizontalRule

defnMary :: (Monad m, FromDoc a) => Phase m -> [Text] -> Text -> m (Maybe a)
defnMary ph cls txt = do
  let mod = getMeAModule txt
  () <- case ph of
    CollPhase -> tell ((++ [Module mod]), mempty)
    EvalPhase -> pure ()
  pure $ do
    guard ("keep" `elem` cls)
    pure $ render (pretty mod)

evalMary :: FromValue b => Text -> MaryM b
evalMary e =
  case parseOnly (topTerm <* endOfInput) e of
    Left err -> error err
    Right t -> do
      is <- gets imports
      fp <- asks filename
      env@(gl,_) <- asks environment
      lcp <- asks commonPrefix
      let t' = fmap (stripVarPrefix lcp) t
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


instance Interpretable Inline Inline where
  interpret ph = \case
    i@(Code attr txt) -> isMaryCode ph attr >>= \case
      (Just cb, attr@(_, cls, _)) -> case cb of
        MaryData -> pure i -- TODO
        MaryDefn -> case ph of
          CollPhase -> do (_ :: Maybe Inline) <- defnMary ph cls txt
                          pure i
          EvalPhase -> fromMaybe nullInline <$> defnMary ph cls txt
        MaryEval -> case ph of
          CollPhase -> pure i
          EvalPhase -> evalMary txt
        FormInput -> case ph of
          CollPhase -> pure i
          EvalPhase -> RawInline (Format "html") <$> makeInputForm False attr txt
      (Nothing, attr) -> pure (Code attr txt)
    Span attr is -> Span attr <$> interpret ph is -- No mary-template here?
    -- structural
    Emph is -> Emph <$> interpret ph is
    Underline is -> Underline <$> interpret ph is
    Strong is -> Strong <$> interpret ph is
    Strikeout is -> Strikeout <$> interpret ph is
    Superscript is -> Superscript <$> interpret ph is
    Subscript is -> Subscript <$> interpret ph is
    SmallCaps is -> SmallCaps <$> interpret ph is
    Quoted qt is -> Quoted qt <$> interpret ph is
    Cite ct is -> Cite ct <$> interpret ph is
    Link attr is tgt -> Link attr <$> interpret ph is <*> makeAbsRef ph tgt
    Image attr is tgt -> Image attr <$> interpret ph is <*> makeAbsRef ph tgt
    Note bs -> Note <$> interpret ph bs
    -- pure
    Str txt -> pure (Str txt)
    Space -> pure Space
    SoftBreak -> pure SoftBreak
    LineBreak -> pure LineBreak
    Math mty txt -> pure (Math mty txt)
    RawInline fmt txt -> pure (RawInline fmt txt)


makeAbsRef :: Phase m -> Target -> m Target
makeAbsRef CollPhase tgt = pure tgt
makeAbsRef EvalPhase (url, title) = do
  absUrl <- if isAbsolute url then pure url -- keep it as is
    else do
      baseURL <- asks baseURL
      page <- asks page
      let thing = if isPub url then "?pub" else "?page"
      -- if current page is eg repo/lectures/bonus/two.md and requested
      -- URL is eg ../../basic/notes.pdf, new URL is repo/basic/notes.pdf
      let newUrl = joinPathT $ normalise (init (T.splitOn "/" page))
                                 (filter (/= ".")  (T.splitOn "/" url))
      pure $ T.concat [baseURL, thing, "=", newUrl]
  pure (absUrl, title)
  where
    isAbsolute t = or (fmap (`T.isPrefixOf` t)
                        ["https://", "http://", "ftp://", "//", "mailto:", "tel:"]) -- TODO: make more generic?
    isPub t      = ("pub/" `T.isPrefixOf` t || "/pub/" `T.isInfixOf` t)
                     && (not $ "pub/" `T.isSuffixOf` t)

    normalise :: [Text] -> [Text] -> [Text]
    normalise (site:_) ("~":us) = normalise [site] us -- '~' => "from site root"
    normalise (site:ps) us = site:go (reverse ps) us -- keep site root always
      where
        go sp [] = reverse sp
        go (_:sp) ("..":us) = go sp us
        go []     ("..":us) = go [] us -- allowing overshooting
        go sp     (p:us)    = go (p:sp) us
    normalise [] _ = error "IMPOSSIBLE: empty page"

    joinPathT = T.pack . joinPath . fmap T.unpack

instance
  ( Interpretable a c
  , Interpretable b d
  ) => Interpretable (a, b) (c, d) where
  interpret ph (a, b) = (,) <$> interpret ph a <*> interpret ph b
  extract (c, d) = (extract c, extract d)

instance Interpretable a b => Interpretable [a] [b] where
  interpret ph = traverse (interpret ph)
  extract = map extract

instance Interpretable a b => Interpretable (Maybe a) (Maybe b) where
  interpret ph = traverse (interpret ph)
  extract = fmap extract

maryDefinitionToModule :: MaryDefinition -> RawModule
maryDefinitionToModule = \case
  Module mod                -> mod
  DivTemplate decl attr div ->
    let funDecl        = (,) <$> identifier <*> argTuple pcomputation
        Right (nm, ps) = parseOnly funDecl decl
    in ([], [(nm, Right (ps :-> [Nothing :?> toRawTerm (Div attr div)]))])

successfully :: Either MaryError a -> IO a
successfully (Left err) = error (show err)
successfully (Right x) = pure x

process :: Pandoc -> IO Pandoc
process rpdoc = do
  (pdoc0, (defns, MaryWriter{..})) <- successfully =<< runMaryCollectM (interpret CollPhase rpdoc)
  let rm@(is, ps) = fold [ (is, p)
                         | ds <- defns
                         , let (is, p) = maryDefinitionToModule ds
                         ]
  (_, env, lcp) <- loadToplevelModule collFilename rm
  let ctx = MaryCtxt
       { commonPrefix = lcp
       , filename = stripPrefixButDot lcp collFilename
       , page = collPage
       , sitesRoot = collSitesRoot
       , baseURL = collBaseURL
       , user = collUser
       , inputs = collInputs
       , environment = (env, collInputs)
       , defaultCodeAttr = (Nothing, mempty)
       }
 -- EnvData is (stripPrefixButDot lcp fp) lcp (env, inputs) baseURL page user
  (pdoc1, _) <- successfully =<< runMaryM ctx (interpret EvalPhase (pdoc0 :: Pandoc))
  pure $ setTitle (fromMaybe "Title TBA" (ala' First query h1 pdoc0))
       . setMeta "jsGlobalEnv" (fromList $ Str <$> jsGlobalEnv env)
       . setMeta "jsInputs" (fromList $ Str <$> jsInputs collInputs)
       $ pdoc1

  where
  h1 :: Block -> Maybe Inlines
  h1 (Header 1 _ is) = Just (fromList is)
  h1 _ = Nothing
