{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE DefaultSignatures     #-}

module Shonkier.Pretty where

import Data.Char
import Data.Foldable
import Data.Function
import Data.List (isPrefixOf, intersperse, groupBy)
import Data.Ratio
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding (Doc, Pretty, pretty, prettyList)
import qualified Data.Text.Prettyprint.Doc as P

import Shonkier.Syntax
import Shonkier.Value
import Utils.List

data Annotation
  = AnnAtom
  | AnnError
  | AnnFunction
  | AnnKeyword
  | AnnNumeric
  | AnnPrimitive
  | AnnString

type Doc = P.Doc Annotation

class Pretty t where
  pretty     :: t -> Doc
  prettyList :: [t] -> Doc

  default pretty :: P.Pretty t => t -> Doc
  pretty = P.pretty

  default prettyList :: Pretty t => [t] -> Doc
  prettyList = list . map pretty

instance Pretty Integer
instance Pretty Text
instance Pretty Char where prettyList = P.prettyList

instance Pretty a => Pretty (Maybe a) where
  pretty = \case
    Nothing -> mempty
    Just a  -> pretty a

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty Rational where
  pretty p =
    let n = numerator p; d = denominator p in
    if | d == 1    -> pretty n
       | d == 2    -> pretty (n `div` 2) <> ".5"
       | d == 4    -> pretty (n `div` 4)
                      <> if n `mod` 4 == 1 then ".25" else ".75"
       | otherwise -> pretty n <+> "/" <+> pretty d

ppAtom :: String -> Doc
ppAtom str = annotate AnnAtom $ case str of
  [] -> "[]"
  a  -> squote <> pretty a

{-
Lisp conventions for ppList:
[xs]           |-> [xs]
[1|[2|[3|xs]]] |-> [1 2 3|xs]
[1|[2|[3|[]]]] |-> [1 2 3]
-}

ppGlobalVar :: Variable -> Doc
ppGlobalVar v
  | "prim" `isPrefixOf` v = annotate AnnPrimitive (pretty v)
  | otherwise             = pretty v

ppList :: Pretty a => ([a], Maybe a) -> Doc
ppList (as, m) = encloseSep lbracket closing space $ pretty <$> as
  where closing = maybe mempty ((pipe <>) . pretty) m <> rbracket

ppApp :: Pretty a => Doc -> [a] -> Doc
ppApp f ts = f <> tupled (pretty <$> ts)

ppFun :: (Pretty a, Pretty b) => [[a]] -> [b] -> Doc
ppFun hs cls = encloseSep lbrace rbrace space $ pretty <$> cls

ppClause :: Pretty v => Clause' v String -> Doc
ppClause ([], t) = pretty t
ppClause (ps, t) = tupled (pretty <$> ps) <+> "->" <+> pretty t

ppStringLit :: String -> T.Text -> Doc
ppStringLit k str = annotate AnnString $
  enclose (key <> dquote) (dquote <> key) (pretty str) where

  tk = T.pack ('"' : k)

  key   = pretty $ case maximum ((-2):occ) of
    (-2) -> k
    n    -> k ++ show (n + 1)
  occ   = [ d | tl <- T.tails str
              , suff <- toList $ T.stripPrefix tk tl
              , let d = mread . T.unpack $ T.takeWhile isDigit suff
              ]
  mread = \case
    [] -> (-1)
    ds -> read ds

instance Pretty Literal where
  pretty = \case
    String k str -> ppStringLit k str
    Num r        -> annotate AnnNumeric $ pretty r

instance Pretty RawVariable where
  pretty (mns, v) = pretty (fmap (++ ".") mns) <> ppGlobalVar v

instance Pretty ScopedVariable where
  pretty = \case
    LocalVar x           -> pretty x
    GlobalVar _ x        -> ppGlobalVar x
    AmbiguousVar _ x     -> annotate AnnError $ pretty x
    OutOfScope x         -> annotate AnnError $ pretty x
    InvalidNamespace _ x -> annotate AnnError $ pretty x

instance Pretty v => Pretty (Term' v String) where
  pretty t = case listView t of
    ([], Just _) -> case t of
      Atom a     -> ppAtom a
      Lit l      -> pretty l
      Var v      -> pretty v
      Cell a b   -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
      App f ts   -> ppApp (pretty f) ts
      Fun hs cls -> ppFun hs cls
    it -> ppList it

instance Pretty v => Pretty (Clause' v String) where
  pretty = ppClause

instance Pretty PValue where
  pretty p = case listView p of
    ([], Just _) -> case p of
      PAtom a   -> ppAtom a
      PLit l    -> pretty l
      PBind v   -> pretty v
      PAs v p   -> pretty v <> "@" <> pretty p
      PWild     -> "_"
      PCell a b -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
    it -> ppList it

instance Pretty PComputation where
  pretty = \case
    PValue p           -> pretty p
    PRequest (a, vs) v -> braces $ hsep [ppApp (ppAtom a) vs, "->", pretty v]
    PThunk v           -> braces $ pretty v

instance Pretty Value where
  pretty v = case listView v of
    ([], Just _) -> case v of
      VAtom a          -> ppAtom a
      VLit l           -> pretty l
      VPrim f _        -> pretty f
      VCell a b        -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
      VFun _ _ hs cls  -> ppFun hs cls
      VThunk c         -> braces $ pretty c
    it -> ppList it

instance Pretty Computation where
  pretty = \case
    Value v             -> pretty v
    Request (a, vs) frs -> ppApp (ppAtom a) vs

instance Pretty v => Pretty (String, Either [[String]] (Clause' v String)) where
  prettyList = vcat
             . intersperse ""
             . map (vcat . map pretty)
             . groupBy ((==) `on` fst)

  pretty (fun, decl) =
    (annotate AnnFunction (pretty fun) <>) $ case decl of
      Left hs  -> tupled $ map (hsep . map pretty) hs
      Right cl -> pretty cl

instance Pretty (FilePath, Maybe Namespace) where
  prettyList = vcat . map pretty

  pretty (fp, mns) =
    annotate AnnKeyword "import" <+> pretty fp
    <+> annotate AnnKeyword (pretty $ ("as" :: String) <$ mns) <+> pretty mns

instance Pretty v => Pretty (Module' v String) where
  pretty (is, p) = pretty is <> pretty p
