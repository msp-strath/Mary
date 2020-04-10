{-# LANGUAGE OverloadedStrings #-}
module Mary.Pandoc where

import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Newtype

import Data.Attoparsec.Text
import Data.Foldable
import Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text as T

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
  let doc2 = walk (evalMaryInline is (env, inputs) page)
           . walk (evalMaryBlock is (env, inputs))
           $ doc1
  pure $ setTitle (fromMaybe "Title TBA" (ala' First query h1 doc0))
       . setMeta "jsGlobalEnv" (fromList $ Str <$> jsGlobalEnv env)
       $ doc2

  where
  h1 :: Block -> Maybe Inlines
  h1 (Header 1 _ is) = Just (fromList is)
  h1 _ = Nothing

metaToInputValues :: Meta -> Map Text Text
metaToInputValues (Meta m) = M.map extract m where
  extract (MetaInlines xs) = T.concat $ L.map inlineToString xs
  extract (MetaString s)        = s
  extract x                     = error $ "IMPOSSIBLE non-string meta value " ++ show x

  inlineToString (Str s)   = s
  inlineToString Space     = " "
  inlineToString SoftBreak = "\n"
  inlineToString LineBreak = "\n"
  inlineToString x         =  error $ "IMPOSSIBLE non-string inline " ++ show x

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
    let funDecl        = (,) <$> identifier <*> tupleOf pcomputation
        Right (nm, ps) = parseOnly funDecl decl
    in ([], [(nm, Right (ps, toRawTerm (Div attr div)))])

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

evalMary :: FromValue b => [Import] -> Env -> Text -> b -> b
evalMary is env e abort =
  case parseOnly (term <* endOfInput) e of
    Left _ -> abort
    Right t -> case rawShonkier is env t of
      Value v -> fromValue v
      Request r _ -> fromValue $ VString "" $ T.pack $ show r -- abort

evalMaryBlock :: [Import] -> Env -> Block -> Block
evalMaryBlock is env (CodeBlock (_, cs, _) e) | "mary" `elem` cs
  = evalMary is env e Null
evalMaryBlock is (env, inputs) (CodeBlock a@(_, cs, as) t) | "input" `elem` cs
    -- we consider codeblocks (compared to inline code) to be
    -- textareas, unless they explicitly have a type set
  = let textarea = "type" `notElem` L.map fst as in
    RawBlock (Format "html") (makeInputForm inputs textarea a t)
evalMaryBlock _ _ b = b

evalMaryInline :: [Import] -> Env -> Text -> Inline -> Inline
evalMaryInline is env page (Code (_, cs, _) e) | "mary" `elem` cs =
  evalMary is env e Space
evalMaryInline is (env,inputs) page (Code a@(_, cs, _) t) | "input" `elem` cs =
  RawInline (Format "html") (makeInputForm inputs False a t)
evalMaryInline _ _ page (Link attrs is target) =
  Link attrs is (makeAbsolute page target)
evalMaryInline _ _ page (Image attrs is target) =
  Image attrs is (makeAbsolute page target)
evalMaryInline _ _ _ b = b

makeInputForm :: M.Map Text Text -> Bool -> Attr -> Text -> Text
makeInputForm _ _ (_, _, as) p | ("type", "submit") `elem` as
  = (T.intercalate " " $
      ["<input"] ++
      [ T.concat [k, "=\"", v, "\""] | (k, v) <- ("value", p):as ]) <> ">"
makeInputForm inputs textarea a@(i, cs, as) p =
  let nameparser = SP.skipSpace *> identifier <* SP.skipSpace in
  case parseOnly nameparser p of
    Left _ -> ""
    Right n -> let name = T.pack n
                   mval = getPOST inputs name in (T.intercalate " " $
      [ if textarea then "<textarea" else "<input"] ++
      [ T.concat [k, "=\"", v, "\""] |
        (k, v) <- ("name",name):("id", name):as ]) <>
      T.concat (if textarea then [">", fromMaybe "" mval , "</textarea>"]
               else [ T.concat [" value=\"", fromJust mval, "\""] | isJust mval] ++  [">"])

makeAbsolute :: Text -> Target -> Target
makeAbsolute page (url, title) = (absUrl, title)
  where
    baseURL = "https://personal.cis.strath.ac.uk/conor.mcbride/shib/Mary/"
    absUrl = if isAbsolute url then -- keep it as is
              url
             else
               T.concat [baseURL, thing, "=", newUrl]
    thing = if isPub url then "?pub" else "?page"
    -- if current page is eg repo/lectures/bonus/two.md and requested
    -- URL is eg ../../basic/notes.pdf, new URL is repo/basic/notes.pdf
    newUrl = joinPathT $ normalise (L.init (splitOn "/" page))
                                   (L.filter (/= ".")  (splitOn "/" url))

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
