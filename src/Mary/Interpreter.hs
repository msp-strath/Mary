module Mary.Interpreter where

import Control.Monad (guard)
import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State  (StateT, runStateT, gets)
import Control.Monad.Reader (ReaderT, runReaderT, local, asks)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Newtype (ala')

import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Bwd (Bwd(..), (<>>))
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (nub, nubBy)
import Data.Monoid (First(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (forM)

import Network.URI.Encode as URI

import Shonkier.Import (loadToplevelModule, stripPrefixButDot, stripVarPrefix)
import Shonkier.ShonkierJS (jsGlobalEnv, jsInputs)
import Shonkier.Pandoc ()
import Shonkier.Parser (getMeAModule, topTerm, identifier, argTuple, pcomputation, skipSpace)
import Shonkier.Pretty (pretty, toString)
import Shonkier.Pretty.Render.Pandoc (render, FromDoc)
import Shonkier.Syntax (Import, RawModule, Clause'((:->)), Rhs'((:?>)), RawTerm, Term'(App), toRawTerm)
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
  | MaryTemplate MaryExpr
  deriving Show


firstBy :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
firstBy p [] = Nothing
firstBy p (a : as)
  | Just b <- p a = pure (b, as)
  | otherwise = fmap (a:) <$> firstBy p as

-- Returns a list without the template
hasMaryTemplate :: [MaryDivAttr] -> Maybe (MaryExpr, [MaryDivAttr])
hasMaryTemplate = firstBy $ \case
  MaryTemplate v -> pure v
  _ -> Nothing

hasCodeDefault :: [Text] -> Maybe [Text]
hasCodeDefault = fmap snd . firstBy (guard . ("code-default" ==))

isMaryDivAttr :: (Text, Text) -> Maybe MaryDivAttr
isMaryDivAttr (k, v)
  | k == "mary-apply" = pure (MaryApply (MaryExpr v))
  | k == "mary-store" = pure (MaryStore (StoreName v))
  | k == "mary-template" = pure (MaryTemplate (MaryExpr v))
  | otherwise = Nothing

unIsMaryDivAttr :: MaryDivAttr -> (Text, Text)
unIsMaryDivAttr = \case
  MaryApply (MaryExpr v) -> ("mary-apply", v)
  MaryStore (StoreName v) -> ("mary-store", v)
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

data MaryCollCtxt = MaryCollCtxt
  { collDefaultCodeAttr :: DefaultCodeAttr
  , collApplyCtxt :: ApplyContext
  }

type MaryCollectM
  = ReaderT MaryCollCtxt
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
  CollPhase -> asks (cast . collDefaultCodeAttr)
  EvalPhase -> asks (cast . defaultCodeAttr)

  where
    cast :: DefaultCodeAttr -> (Maybe MaryCodeAttr, Attr)
    cast (mb, (cls1, kvs1))
      = ( mb
        , ( id0
          , nub (cls0 <> cls1)
          , nubBy ((==) `on` fst) (kvs0 <> kvs1)))

type ApplyContext
  = Bwd ( MaryExpr -- the raw expression
        , RawTerm  -- its parsed version
        )

asksApplyContext :: Phase m -> (ApplyContext -> a) -> m a
asksApplyContext CollPhase f = asks (f . collApplyCtxt)
asksApplyContext EvalPhase f = asks (f . applyCtxt)

localApplyContext :: Phase m -> (ApplyContext -> ApplyContext) -> m a -> m a
localApplyContext CollPhase f = local (\ r -> r { collApplyCtxt = f (collApplyCtxt r) })
localApplyContext EvalPhase f = local (\ r -> r { applyCtxt = f (applyCtxt r) })

data MaryCtxt = MaryCtxt
  { commonPrefix :: String
  , filename :: FilePath
  , page :: Text
  , sitesRoot :: Text
  , baseURL :: Text
  , user :: Maybe Text
  , inputs :: Map Text Text
  , environment :: Env
  -- collected info
  , defaultCodeAttr :: DefaultCodeAttr
  , applyCtxt :: ApplyContext
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
  . flip runReaderT (MaryCollCtxt (Nothing, mempty) B0)

data Phase m where
  CollPhase :: Phase MaryCollectM
  EvalPhase :: Phase MaryM

class Interpretable a b where
  interpret :: Monad m => Phase m -> a -> m b
  extract :: b -> a

  default extract :: b ~ a => b -> a
  extract = id

interprets :: (Interpretable a c, c ~ Many b, Monad m, Monoid (Many b))
           => Phase m -> [a] -> m [b]
interprets ph as = toList . mconcat <$> interpret ph as

interpretss :: (Interpretable a c, c ~ Many b, Monad m, Monoid (Many b))
            => Phase m -> [[a]] -> m [[b]]
interpretss ph ass = fmap (toList . mconcat) <$> interpret ph ass

interpretsss :: (Interpretable a c, c ~ Many b, Monad m, Monoid (Many b))
            => Phase m -> [[[a]]] -> m [[[b]]]
interpretsss ph ass = fmap (fmap (toList . mconcat)) <$> interpret ph ass

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
    Pandoc meta <$> interprets ph docs

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
  interpret ph (Caption ms bs) = do
    (ms :: Maybe [Inline]) <- traverse (interprets ph) ms
    Caption ms <$> interprets ph bs

instance Interpretable Cell Cell where
  interpret ph (Cell attr alg rsp csp bs)
    = Cell attr alg rsp csp <$> interprets ph bs

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

withDefaultCodeAttr :: Phase m -> DefaultCodeAttr -> m a -> m a
withDefaultCodeAttr CollPhase v = local (\ r -> r { collDefaultCodeAttr =  v })
withDefaultCodeAttr EvalPhase v = local (\ r -> r { defaultCodeAttr = v })

-- NB: also used for spans
isMaryDiv  :: Monad m => Phase m -> Attr
           -> m (Either DefaultCodeAttr ([MaryDivAttr], Attr))
isMaryDiv ph attr = do
  -- /!\ This interpret better be pure (apart from raising errors)!
  ((mc, ds), attr@(i, cs, kvs)) <- interpret ph attr
  case hasCodeDefault cs of
    Just cs -> pure (Left (mc, (cs, kvs)))
    Nothing -> case mc of
      Nothing -> pure (Right (ds, attr))
      Just b -> throwMaryError ph (CodeAttributeInADivBlock b)

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

nullBlock :: Block
nullBlock = Plain []

nullInline :: Inline
nullInline = Str ""

actOnMaryDivAttr :: Monad m => Block
                 -> Phase m
                 -> Bwd MaryDivAttr -> [MaryDivAttr]
                 -> Attr -> [Block] -> m Blocks
actOnMaryDivAttr i ph dz [] attr bs
  = case ph of
      CollPhase -> singleton . Div (unIsMaryDiv (dz <>> []) attr) <$> interprets ph bs
      EvalPhase -> case attr of
        -- only reinstate the div if there are interesting attributes
        ("", [], []) -> mconcat <$> interpret ph bs
        _ -> singleton . Div attr <$> interprets ph bs
actOnMaryDivAttr i ph dz (d : ds) attr bs = case d of
  MaryTemplate decl -> case ph of
    CollPhase -> do
      -- templates get shoved into the global env and so should grab
      -- *all* the contextualising info
      funs <- asksApplyContext ph (fmap (MaryApply . fst))
      let ds' = funs <>> ds
      tell ( (DivTemplate (getMaryExpr decl) (unIsMaryDiv ds' attr) bs :)
           , First Nothing)
      pure (singleton i)
    EvalPhase -> pure mempty
  MaryApply me ->
    case parseOnly (topTerm <* endOfInput) (getMaryExpr me) of
      Left err -> error err
      Right t -> localApplyContext ph (:< (me, t)) $
        actOnMaryDivAttr i ph (dz :< MaryApply me) ds attr bs
  MaryStore st -> undefined

instance Interpretable Block Blocks where
  interpret ph = \case
    i@(CodeBlock attr txt) -> isMaryCode ph attr >>= \case
      (Just cb, attr@(_, cls, kvs)) -> case cb of
        MaryData -> pure (singleton i) -- TODO
        MaryDefn -> case ph of
          CollPhase -> do (_ :: Blocks) <- defnMary ph cls txt
                          pure (singleton i)
          EvalPhase -> defnMary ph cls txt
        MaryEval -> case ph of
          CollPhase -> pure (singleton i)
          EvalPhase -> singleton <$> evalMary txt
        FormInput -> case ph of
          CollPhase -> pure (singleton i)
          EvalPhase ->
            let textarea = "type" `notElem` map fst kvs in
            singleton . RawBlock (Format "html") <$> makeInputForm textarea attr txt
      (Nothing, attr) -> pure (singleton $ CodeBlock attr txt)
    i@(Div attr bs) -> isMaryDiv ph attr >>= \case
      Left dca -> withDefaultCodeAttr ph dca $ do
        (bs :: Blocks) <- mconcat <$> interpret ph bs
        pure $ case ph of
          CollPhase -> singleton (Div attr $ toList bs)
          EvalPhase -> bs
      Right (ds, attr) -> actOnMaryDivAttr i ph B0 ds attr bs
    -- structural
    Plain is -> singleton . Plain <$> interprets ph is
    Para is -> singleton .  Para <$> interprets ph is
    LineBlock iss -> singleton . LineBlock <$> interpretss ph iss
    BlockQuote bs -> singleton . BlockQuote <$> interprets ph bs
    OrderedList lattr bss -> singleton .  OrderedList lattr <$> interpretss ph bss
    BulletList bss -> singleton .  BulletList <$> interpretss ph bss
    DefinitionList ibsss -> do
      ibsss <- forM ibsss $ \ (is, bss) ->
        (,) <$> interprets ph is <*> interpretss ph bss
      pure (singleton $ DefinitionList ibsss)
    Header i attr is -> singleton .  Header i attr <$> interprets ph is
    Table attr capt cols th tds tf -> fmap singleton $
      Table attr <$> interpret ph capt
                 <*> pure cols
                 <*> interpret ph th
                 <*> interpret ph tds
                 <*> interpret ph tf
    Figure attr cap bs -> fmap singleton $
      Figure attr <$> interpret ph cap <*> interprets ph bs
    -- pure
    RawBlock fmt txt -> pure (singleton $ RawBlock fmt txt)
    HorizontalRule -> pure (singleton HorizontalRule)

  extract bs = case toList bs of
    [] -> nullBlock
    [x] -> x
    xs -> Div ("", [], []) xs

defnMary :: (Monoid (Many a), Monad m, FromDoc a)
         => Phase m -> [Text] -> Text -> m (Many a)
defnMary ph cls txt = do
  let mod = getMeAModule txt
  () <- case ph of
    CollPhase -> tell ((++ [Module mod]), mempty)
    EvalPhase -> pure ()
  pure $ if "keep" `elem` cls
    then singleton $ render (pretty mod)
    else mempty

evalMary :: FromValue b => Text -> MaryM b
evalMary e =
  case parseOnly (topTerm <* endOfInput) e of
    Left err -> error err
    Right t0 -> do
      -- contextualise the term
      funs <- asksApplyContext EvalPhase id
      let t1 = applyFuns funs t0
      -- get the environment
      is <- gets imports
      fp <- asks filename
      env@(gl,_) <- asks environment
      lcp <- asks commonPrefix
      -- eval the term
      let t2 = fmap (stripVarPrefix lcp) t1
      go env (rawShonkier is fp gl t2)
  where
  applyFuns :: ApplyContext -> RawTerm -> RawTerm
  applyFuns B0 t = t
  applyFuns (funs :< (_, f)) t = applyFuns funs (App f [t])

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


instance Interpretable Inline Inlines where
  interpret ph = \case
    i@(Code attr txt) -> isMaryCode ph attr >>= \case
      (Just cb, attr@(_, cls, _)) -> case cb of
        MaryData -> pure (singleton i) -- TODO
        MaryDefn -> case ph of
          CollPhase -> do (_ :: Inlines) <- defnMary ph cls txt
                          pure (singleton i)
          EvalPhase -> defnMary ph cls txt
        MaryEval -> case ph of
          CollPhase -> pure (singleton i)
          EvalPhase -> singleton <$> evalMary txt
        FormInput -> case ph of
          CollPhase -> pure (singleton i)
          EvalPhase -> singleton . RawInline (Format "html") <$> makeInputForm False attr txt
      (Nothing, attr) -> pure (singleton $ Code attr txt)
    Span attr is -> singleton . Span attr <$> interprets ph is -- No mary-template here?
    -- structural
    Emph is -> singleton . Emph <$> interprets ph is
    Underline is -> singleton . Underline <$> interprets ph is
    Strong is -> singleton . Strong <$> interprets ph is
    Strikeout is -> singleton . Strikeout <$> interprets ph is
    Superscript is -> singleton . Superscript <$> interprets ph is
    Subscript is -> singleton . Subscript <$> interprets ph is
    SmallCaps is -> singleton . SmallCaps <$> interprets ph is
    Quoted qt is -> singleton . Quoted qt <$> interprets ph is
    Cite ct is -> singleton . Cite ct <$> interprets ph is
    Link attr is tgt -> fmap singleton $
      Link attr <$> interprets ph is <*> makeAbsRef ph tgt
    Image attr is tgt -> fmap singleton $
      Image attr <$> interprets ph is <*> makeAbsRef ph tgt
    Note bs -> singleton . Note <$> interprets ph bs
    -- pure
    Str txt -> pure (singleton $ Str txt)
    Space -> pure (singleton Space)
    SoftBreak -> pure (singleton SoftBreak)
    LineBreak -> pure (singleton LineBreak)
    Math mty txt -> pure (singleton $ Math mty txt)
    RawInline fmt txt -> pure (singleton $ RawInline fmt txt)

  extract is = case toList is of
    [] -> nullInline
    [i] -> i
    is -> Span ("", [], []) is

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
       , applyCtxt = B0
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
