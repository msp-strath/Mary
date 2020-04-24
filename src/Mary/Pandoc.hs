{-# LANGUAGE OverloadedStrings #-}
module Mary.Pandoc where

import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Monad.Reader (Reader, runReader, asks)
import Control.Newtype

import Data.Attoparsec.Text
import Data.Foldable
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (First(..))
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text as T

import Network.URI.Encode as URI

import Text.Pandoc.Builder
import Text.Pandoc.Walk

import System.FilePath

import Shonkier.Import
import Shonkier.Parser as SP
import Shonkier.Pretty
import Shonkier.Pretty.Render.Pandoc
import Shonkier.Semantics
import Shonkier.ShonkierJS
import Shonkier.Syntax
import Shonkier.Value
import Shonkier.Pandoc()

process :: Pandoc -> IO Pandoc
process doc0@(Pandoc meta docs) = do
  let (doc1, defns) = runWriter (walkM snarfMaryDef doc0)
  let rm@(is, ps)  = fold [ (is, p)
                          | ds <- defns
                          , let (is, p) = maryDefinitionToModule ds
                          ]
  (mod, env) <- loadToplevelModule "." rm

  -- we assume that there is page metadata, put there by mary find
  let inputs = metaToInputValues meta
  let page = flip fromMaybe (getGET inputs "page")
               $ error "Meta data 'page' missing!"
  let baseURL = flip fromMaybe (M.lookup "baseURL" inputs)
               $ error "Meta data 'baseURL' missing!"
  let user = M.lookup "user" inputs
  let envdata = EnvData is (env, inputs) baseURL page user
  let doc2 = runReader (walkM evalMaryBlock  doc1) envdata
  let doc3 = runReader (walkM evalMaryInline doc2) envdata
  pure $ setTitle (fromMaybe "Title TBA" (ala' First query h1 doc0))
       . setMeta "jsGlobalEnv" (fromList $ Str <$> jsGlobalEnv env)
       . setMeta "jsInputs" (fromList $ Str <$> jsInputs inputs)
       $ doc3

  where
  h1 :: Block -> Maybe Inlines
  h1 (Header 1 _ is) = Just (fromList is)
  h1 _ = Nothing

metaToInputValues :: Meta -> Map Text Text
metaToInputValues (Meta m) = M.map extract m where
  extract (MetaInlines xs) = T.concat $ L.map inlineToString xs
  extract x                                          = error $
    "IMPOSSIBLE non-string meta value " ++ show x

  inlineToString (RawInline (Format "html") s)   = URI.decodeText s
  inlineToString (Str s)                        = s
  inlineToString x         =  error $
    "IMPOSSIBLE non-string inline " ++ show x

{-
  extract (MetaBlocks xs)  = T.concat $ L.map blockToString xs
  extract (MetaString s)   = s

  inlineToString Space     = " "
  inlineToString SoftBreak = "\n"
  inlineToString LineBreak = "\n"

  blockToString (Plain xs)      = T.concat $ L.map inlineToString xs
  blockToString (Para xs)       = T.concat $ L.map inlineToString xs
  blockToString (LineBlock xss) = T.concat $ L.concatMap (L.map inlineToString) xss
  blockToString x               =  error $ "IMPOSSIBLE non-string block " ++ show x
-}

getGET :: Map Text Text -> Text -> Maybe Text
getGET inputs x = M.lookup ("GET_" <> x) inputs

getPOST :: Map Text Text -> Text -> Maybe Text
getPOST inputs x = M.lookup ("POST_" <> x) inputs

data MaryDefinition
  = Module RawModule
  | DivTemplate Text Attr [Block]

maryDefinitionToModule :: MaryDefinition -> RawModule
maryDefinitionToModule = \case
  Module mod                -> mod
  DivTemplate decl attr div ->
    let funDecl        = (,) <$> identifier <*> argTuple pcomputation
        Right (nm, ps) = parseOnly funDecl decl
    in ([], [(nm, Right (ps, [Nothing :?> toRawTerm (Div attr div)]))])

snarfMaryDef :: Block -> Writer [MaryDefinition] Block
snarfMaryDef c@(CodeBlock (_, cs, _) p)
  | "mary-def" `elem` cs
  = do let mod = getMeAModule p
       let out = if "keep" `notElem` cs then Null else render (pretty mod)
       out <$ tell [Module mod]
snarfMaryDef c@(Div (a , b, kvs) p)
  | Just decl <- lookup "mary" kvs
  = let attr = (a, b, L.filter (("mary" /=) . fst) kvs) in
    Null <$ tell [DivTemplate decl attr p]
snarfMaryDef b = return b

data EnvData = EnvData { imps        :: [Import]
                       , environment :: Env
                       , baseURL     :: Text
                       , page        :: Text
                       , user        :: Maybe Text
                       }

type EnvReader a = Reader EnvData a

readImports :: EnvReader [Import]
readImports = asks imps

readEnv :: EnvReader Env
readEnv = asks environment

readBaseURL :: EnvReader Text
readBaseURL = asks baseURL

readPage :: EnvReader Text
readPage = asks page

evalMary :: FromValue b => Text -> EnvReader b
evalMary e =
  case parseOnly (topTerm <* endOfInput) e of
    Left err -> error err
    Right t -> do
      is <- readImports
      env <- readEnv
      case rawShonkier is env t of
        Value v -> pure $ fromValue v
        Request r _ -> error (show r)



evalMaryBlock :: Block -> EnvReader Block
evalMaryBlock (CodeBlock (_, cs, _) e) | "mary" `elem` cs = evalMary e
evalMaryBlock (CodeBlock a@(_, cs, as) t) | "input" `elem` cs
    -- we consider codeblocks (compared to inline code) to be
    -- textareas, unless they explicitly have a type set
  = let textarea = "type" `notElem` L.map fst as in
    RawBlock (Format "html") <$> (makeInputForm textarea a t)
evalMaryBlock b = pure b

evalMaryInline :: Inline -> EnvReader Inline
evalMaryInline (Code (_, cs, _) e) | "mary" `elem` cs = evalMary e
evalMaryInline (Code a@(_, cs, _) t) | "input" `elem` cs =
  RawInline (Format "html") <$> makeInputForm False a t
evalMaryInline (Link attrs is target)  = Link attrs is <$> makeAbsolute target
evalMaryInline (Image attrs is target) = Image attrs is <$> makeAbsolute target
evalMaryInline b = pure b


makeInputForm :: Bool -> Attr -> Text -> EnvReader Text
makeInputForm _ (_, _, as) p | ("type", "submit") `elem` as
  = pure $ (T.intercalate " " $
      ["<input"] ++
      [ T.concat [k, "=\"", v, "\""] | (k, v) <- ("value", p):as ]) <> ">"
makeInputForm textarea a@(i, cs, as) p = do
  (_,inputs) <- readEnv
  let nameparser = SP.skipSpace *> identifier <* SP.skipSpace
  pure $ case parseOnly nameparser p of
    Left _ -> ""
    Right n -> let name = T.pack n
                   mval = getPOST inputs name in (T.intercalate " " $
      [ if textarea then "<textarea" else "<input"] ++
      [ T.concat [k, "=\"", v, "\""] |
        (k, v) <- ("name",name):("id", name):as ]) <>
      T.concat (if textarea then [">", fromMaybe "" mval , "</textarea>"]
               else [ T.concat [" value=\"", fromJust mval, "\""] | isJust mval] ++  [">"])

makeAbsolute :: Target -> EnvReader Target
makeAbsolute (url, title) = do
  absUrl <- if isAbsolute url then pure url -- keep it as is
    else do
      baseURL <- readBaseURL
      page <- readPage
      let thing = if isPub url then "?pub" else "?page"
      -- if current page is eg repo/lectures/bonus/two.md and requested
      -- URL is eg ../../basic/notes.pdf, new URL is repo/basic/notes.pdf
      let newUrl = joinPathT $ normalise (L.init (splitOn "/" page))
                                 (L.filter (/= ".")  (splitOn "/" url))
      pure $ T.concat [baseURL, thing, "=", newUrl]
  pure (absUrl, title)
  where
    isAbsolute t = or (fmap (`T.isPrefixOf` t)
                        ["https://", "http://", "ftp://", "//", "mailto:", "tel:"]) -- TODO: make more generic?
    isPub t      = ("pub/" `T.isPrefixOf` t || "/pub/" `T.isInfixOf` t)
                     && (not $ "pub/" `T.isSuffixOf` t)

    normalise :: [Text] -> [Text] -> [Text]
    normalise (site:_) ("~":us) = normalise [site] us -- '~' => "from site root"
    normalise (site:ps) us = site:go (L.reverse ps) us -- keep site root always
      where
        go sp [] = L.reverse sp
        go (_:sp) ("..":us) = go sp us
        go []     ("..":us) = go [] us -- allowing overshooting
        go sp     (p:us)    = go (p:sp) us
    normalise [] _ = error "IMPOSSIBLE: empty page"

    joinPathT = T.pack . joinPath . fmap T.unpack
