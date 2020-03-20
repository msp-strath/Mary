{-# LANGUAGE OverloadedStrings #-}
module Mary.Pandoc where

import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Newtype

import Data.Attoparsec.Text
import Data.Foldable
import Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Text as T

import Text.Pandoc.Builder
import Text.Pandoc.Walk

import System.FilePath

import Shonkier.Parser
import Shonkier.Value
import Shonkier.Import
import Shonkier.Syntax
import Shonkier.Semantics
import Shonkier.ShonkierJS

process :: Pandoc -> IO Pandoc
process doc0@(Pandoc meta docs) = do
  let (doc1, defns) = runWriter (walkM snarfMaryDef doc0)
  let rm@(is, ps)  = fold [ (is, p)
                          | ds <- defns
                          , let Right (is, p) = parseOnly module_ ds
                          ]
  (mod, env) <- loadToplevelModule "." rm
  -- we assume that there is page metadata, put there by mary find
  let page = case lookupMeta "page" meta of
        Just (MetaInlines [Str s]) -> s
        Just (MetaString s) -> s
        _ -> error $ "Meta data 'page' missing! Meta:" ++ show meta
  let doc2 = walk (evalMaryInline is env page)
           . walk (evalMaryBlock is env)
           $ doc1
  pure $ setTitle (fromMaybe "Title TBA" (ala' First query h1 doc0))
       . setMeta "jsGlobalEnv" (fromList $ Str <$> jsGlobalEnv env)
       $ doc2

  where

  h1 :: Block -> Maybe Inlines
  h1 (Header 1 _ is) = Just (fromList is)
  h1 _ = Nothing


snarfMaryDef :: Block -> Writer [Text] Block
snarfMaryDef c@(CodeBlock (_, cs, _) p)
  | "mary-def" `elem` cs
  = if "keep" `elem` cs
    then c <$ tell [p]
    else Null <$ tell [p]
snarfMaryDef b = return b

evalMaryBlock :: [Import] -> GlobalEnv -> Block -> Block
evalMaryBlock is env (CodeBlock (_, cs, _) e) | "mary" `elem` cs =
  case parseOnly (term <* endOfInput) e of
    Left _ -> Null
    Right t -> case rawShonkier is env t of
      Value v -> fromValue v
      _ -> Null
evalMaryBlock _ _ b = b

evalMaryInline :: [Import] -> GlobalEnv -> Text -> Inline -> Inline
evalMaryInline is env page (Code (_, cs, _) e) | "mary" `elem` cs =
  case parseOnly (term <* endOfInput) e of
    Left _ -> Space
    Right t -> case rawShonkier is env t of
      Value v -> fromValue v
      _ -> Space
evalMaryInline _ _ page (Link attrs is target) =
  Link attrs is (makeAbsolute page target)
evalMaryInline _ _ page (Image attrs is target) =
  Image attrs is (makeAbsolute page target)
evalMaryInline _ _ _ b = b

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

listy :: (FromValue a) => ([a] -> b) -> Value -> b
listy = (. fromValue)

after1Listy :: (FromValue a, FromValue b) => (a -> b -> c) -> Value -> c
after1Listy c (VCell a b) = c (fromValue a) (fromValue b)
after1Listy c _ = after1Listy c (VCell VNil VNil)

after2Listy :: (FromValue a, FromValue b, FromValue c)
        => (a -> b -> c -> d) -> Value -> d
after2Listy c (VCell a bc) = after1Listy (c (fromValue a)) bc
after2Listy c _ = after2Listy c (VCell VNil (VCell VNil VNil))

takes1 :: FromValue a => (a -> b) -> Value -> b
takes1 f (VCell a _) = f (fromValue a)
takes1 f _ = f (fromValue VNil)

takes2 :: (FromValue a, FromValue b) => (a -> b -> c) -> Value -> c
takes2 f (VCell a x) = takes1 (f (fromValue a)) x
takes2 f v = takes1 (f (fromValue VNil)) v

takes3 :: (FromValue a, FromValue b, FromValue c) => (a -> b -> c -> d) -> Value -> d
takes3 f (VCell a x) = takes2 (f (fromValue a)) x
takes3 f v = takes2 (f (fromValue VNil)) v

instance FromValue Int where
  fromValue (VNum d) = truncate d
  fromValue _ = 0

instance FromValue Text where
  fromValue (VString _ s) = s
  fromValue _ = ""

instance FromValue Format where
  fromValue = Format . fromValue

instance FromValue ListNumberDelim where
  fromValue (VAtom tag) = case tag of
    "DefaultDelim" -> DefaultDelim
    "Period"       -> Period
    "OneParen"     -> OneParen
    "TwoParens"    -> TwoParens
    _              -> DefaultDelim
  fromValue _ = DefaultDelim

instance FromValue ListNumberStyle where
  fromValue (VAtom tag) = case tag of
    "DefaultStyle" -> DefaultStyle
    "Example"      -> Example
    "Decimal"      -> Decimal
    "LowerRoman"   -> LowerRoman
    "UpperRoman"   -> UpperRoman
    "LowerAlpha"   -> LowerAlpha
    "UpperAlpha"   -> UpperAlpha
    _              -> DefaultStyle
  fromValue _ = DefaultStyle

instance FromValue Block where
  fromValue (VCell (VAtom tag) is) = ($ is) $ case tag of
    "Plain"          -> listy Plain
    "Para"           -> listy Para
    "LineBlock"      -> listy LineBlock
    "CodeBlock"      -> takes2 CodeBlock
    "RawBlock"       -> takes2 RawBlock
    "BlockQuote"     -> listy BlockQuote
    "OrderedList"    -> after1Listy OrderedList
    "BulletList"     -> listy BulletList
    "DefinitionList" -> listy DefinitionList
    "Header"         -> after2Listy Header
    "HorizontalRule" -> const HorizontalRule
    -- TODO: Table
    "Div"            -> after1Listy Div
    "Null"           -> const Null
    _                -> const Null
  fromValue _ = Null

instance FromValue Inline where
  fromValue (VString _ t) = Str t
  fromValue (VCell (VAtom tag) is) = ($ is) $ case tag of
    "Emph"        -> listy Emph
    "Strong"      -> listy Strong
    "StrikeOut"   -> listy Strikeout
    "Superscript" -> listy Superscript
    "Subscript"   -> listy Subscript
    "SmallCaps"   -> listy SmallCaps
    -- TODO: Quoted
    -- TODO: Cite
    "Code"        -> takes2 Code
    "SoftBreak"   -> const SoftBreak
    "LineBreak"   -> const SoftBreak
    -- TODO: Math
    "RawInline"   -> takes2 RawInline
    "Link"        -> takes3 Link
    "Image"       -> takes3 Image
    "Note"        -> listy Note
    "Span"        -> after1Listy Span
    _             -> const Space
  fromValue _ = Space
