{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE DefaultSignatures     #-}

module Shonkier.Pretty where

import Data.Char
import Data.Foldable
import Data.Function
import Data.List (isPrefixOf, intersperse, groupBy, stripPrefix)
import Data.Ratio
import Data.Semigroup ((<>)) -- needed for ghc versions <= 8.2.2
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding (Doc, Pretty, pretty, prettyList, semi)
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
  | AnnBoolean  -- wasn't she married to Henry VIII?
  | AnnOperator
  | AnnPrimitive
  | AnnSplice
  | AnnString

type Doc = P.Doc Annotation


class Pretty t where
  pretty     :: t -> Doc
  prettyList :: [t] -> Doc
  prettyPrec :: WhereAmI -> t -> Doc

  default pretty :: P.Pretty t => t -> Doc
  pretty = P.pretty

  default prettyList :: Pretty t => [t] -> Doc
  prettyList = list . map pretty

  -- by default, ignore precedence
  default prettyPrec :: Pretty t => WhereAmI -> t -> Doc
  prettyPrec _ = pretty

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
  pretty = prettyPrec Utopia
  prettyPrec z r = pretty (ppRational z r)

parensIf :: OpFax -> WhereAmI -> Doc -> Doc
parensIf o w = if needParens o w then parens else id

ppRational :: WhereAmI -> Rational -> Text
ppRational w p = T.pack $
    let n = numerator p; d = denominator p in
    if | d == 1    -> show n
       | d == 2    -> show (n `div` 2) <> ".5"
       | d == 4    -> show (n `div` 4)
                      <> if n `mod` 4 == 1 then ".25" else ".75"
       | otherwise -> par w $ show n <> "/" <> show d
  where
    par w s
      | needParens overFax w = "(" <> s <> ")"
      | otherwise            = s

ppAtom :: String -> Doc
ppAtom str = annotate AnnAtom $ case str of
  a  -> squote <> pretty a

arrow :: Doc
arrow = annotate AnnOperator "->"

arobase :: Doc
arobase = annotate AnnOperator "@"

assignment :: Doc
assignment = annotate AnnOperator ":="

prioritize :: Doc
prioritize = annotate AnnOperator "?>"

mask :: Doc
mask = annotate AnnOperator "^"

semi :: Doc
semi = annotate AnnOperator P.semi

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
ppFun hs [cl] = enclose lbrace rbrace $ pretty cl
ppFun hs cls  = hang 0 $ enclose lbrace rbrace
              $ (hang 0 $ pretty cls) <> line

ppClause :: (Pretty v, InfixHuh v) => Clause' String v -> Doc
ppClause ([], [Nothing :?> t])  = pretty t
ppClause (ps, rs) = hsep (pretty <$> ps) <+> ppRights rs

ppRights :: (Pretty v, InfixHuh v) => [Rhs' String v] -> Doc
ppRights [Nothing :?> t] = hsep [arrow, pretty t]
ppRights rs              = hsep (map pretty rs)

ppSplice :: Pretty a => Keyword -> [(Text, a)] -> Text -> Doc
ppSplice k tas u = annotate AnnString $
  let key  = mkKeyword k (u : map fst tas) in
  let open = key <> "`"; close = "`" <> key in
  enclose (key <> dquote) (dquote <> key) $
    foldMap (\ (t, a) -> pretty t <> annotate AnnSplice (open <> pretty a <> close)) tas <> pretty u

ppStringLit :: Keyword -> Text -> Doc
ppStringLit k str = annotate AnnString $
  let key = mkKeyword k [str] in
  enclose (key <> dquote) (dquote <> key) (pretty str)

mkKeyword :: Keyword -> [Text] -> Doc
mkKeyword [] ts
    -- if we're forced to vary the empty keyword, we need to kick off with an alpha
  | any (T.any (`elem` ['"', '`'])) ts = mkKeyword "quo" ts
  | otherwise = mempty
mkKeyword k ts = pretty $ case maximum (maximum ((-2):occ) : qso) of
  (-2) -> k
  n    -> k ++ show (n + 1)

  where

  haystack = ts >>= T.tails

  kt  = T.pack k
  uqk = T.pack ('"' : k)
  usk = T.pack ('`' : k)

  occ   = [ d | tl <- haystack
              , suff <- [T.stripPrefix uqk tl, T.stripPrefix usk tl] >>= toList
              , let d = mread $ T.takeWhile isDigit suff
              ]

  qso   = [ mread d
          | tl <- haystack
          , suff <- toList $ T.stripPrefix kt tl
          , let (d, r) = T.span isDigit suff
          , case T.uncons r of { Just ('`', _) -> True ; _ -> False }
          ]

  mread t = case T.unpack t of
    [] -> (-1)
    ds -> read ds

instance Pretty Literal where
  pretty = prettyPrec Utopia
  prettyPrec w = \case
    Num r        -> annotate AnnNumeric $ prettyPrec w r
    Boolean b
      | b         -> annotate AnnBoolean "'1"
      | otherwise -> annotate AnnBoolean "'0"

instance Pretty RawVariable where
  pretty (mns, v) = pretty (fmap (++ ".") mns) <> ppGlobalVar v

instance Pretty ScopedVariable where
  pretty (sco :.: x) = case sco of
    LocalVar           -> pretty x
    GlobalVar b _      -> if b then ppGlobalVar x else pretty x
    AmbiguousVar _     -> annotate AnnError $ pretty x
    OutOfScope         -> annotate AnnError $ pretty x
    InvalidNamespace _ -> annotate AnnError $ pretty x

instance (Pretty v, InfixHuh v) => Pretty (Term' String v) where
  pretty = prettyPrec Utopia
  prettyPrec w t = case listView t of
    ([], Just _) -> case t of
      Atom a        -> ppAtom a
      Lit l         -> prettyPrec w l
      String k ps t -> ppSplice k ps t
      Var v         -> pretty v
      Nil           -> error "The IMPOSSIBLE happened! listView refused to eat a nil."
      Cell a b      -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
      App f ts      -> case (f, ts) of
        (Var v, [l, r])  -- FIXME: that _ is suspicious
          | Just (o, f) <- infixHuh v
          -> parensIf f w $
             prettyPrec (LeftOf :^: f) l -- FIXME: left associativity baked in
             <+> annotate AnnOperator (pretty o)
             <+> prettyPrec (RightOf :^: f) r
        _ -> ppApp (prettyPrec (LeftOf :^: applFax) f) ts
      Semi l r      -> parensIf semiFax w $
        prettyPrec (LeftOf :^: semiFax) l <> semi <+> prettyPrec (RightOf :^: semiFax) r
      Prio l r      -> parensIf prioFax w $
        prettyPrec (LeftOf :^: prioFax) l <+> prioritize <+> prettyPrec (RightOf :^:prioFax) r
      Fun hs cls    -> ppFun hs cls
      Match p t     -> parensIf pamaFax w $
        pretty p <+> assignment <+> prettyPrec (RightOf :^: pamaFax) t
      Mask a t      -> parensIf maskFax w $
        ppAtom a <+> mask <+> prettyPrec (RightOf :^: maskFax) t
    it -> ppList it

instance (Pretty v, InfixHuh v) => Pretty (Clause' String v) where
  pretty = ppClause

  prettyList = vcat . map pretty

instance (Pretty v, InfixHuh v) => Pretty (Rhs' String v) where
  pretty (mg :?> t) = hsep (pipe : foldMap (pure . pretty) mg ++ [arrow, pretty t])

instance Pretty PValue where
  pretty p = case listView p of
    ([], Just _) -> case p of
      PAtom a        -> ppAtom a
      PLit l         -> pretty l
      PString k ps t -> ppSplice k ps t
      PBind v        -> pretty v
      PAs v p        -> pretty v <> arobase <> pretty p
      PWild          -> "_"
      PNil           -> error "The IMPOSSIBLE happened! listView refused to eat a nil."
      PCell a b      -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
    it -> ppList it

instance Pretty PComputation where
  pretty = \case
    PValue p           -> pretty p
    PRequest (a, vs) v -> braces $ hsep [ppApp (ppAtom a) vs, arrow, pretty v]
    PThunk v           -> braces $ pretty v

instance Pretty Value where
  pretty v = case listView v of
    ([], Just _) -> case v of
      VAtom a          -> ppAtom a
      VLit l           -> pretty l
      VString k t      -> ppStringLit k t
      VPrim f _        -> pretty f
      VNil             -> error "The IMPOSSIBLE happened! listView refused to eat a nil."
      VCell a b        -> error "The IMPOSSIBLE happened! listView refused to eat a cell."
      VFun _ _ hs cls  -> ppFun hs cls
      VThunk c         -> braces $ pretty c
    it -> ppList it

instance Pretty Computation where
  pretty = \case
    Value v             -> pretty v
    Request (a, vs) frs -> ppApp (ppAtom a) vs

instance (Pretty v, InfixHuh v) => Pretty (String, Either [[String]] (Clause' String v)) where
  prettyList = vcat
             . intersperse ""
             . map (vcat . map pretty)
             . groupBy ((==) `on` fst)

  pretty (fun, decl) =
    (annotate AnnFunction (pretty fun) <>) $ case decl of
      Left hs        -> tupled $ map (hsep . map pretty) hs
      Right (ps, rs) -> tupled (pretty <$> ps) <+> ppRights rs

instance Pretty (FilePath, Maybe Namespace) where
  prettyList = vcat . map pretty

  pretty (fp, mns) =
    annotate AnnKeyword "import" <+> pretty fp
    <+> annotate AnnKeyword (pretty $ ("as" :: String) <$ mns) <+> pretty mns

instance (Pretty v, InfixHuh v) => Pretty (Module' String v) where
  pretty (is, p) = pretty is <> pretty p


------------------------------------------------------------------------------
-- can tell if it's infix

class InfixHuh v where
  infixHuh :: v -> Maybe (String, OpFax)

instance InfixHuh Variable where
  infixHuh x
    | Just i <- stripPrefix "primInfix" x
    , [x] <- [x | x@(_, y) <- infixOpFax, spell y == i]
    = Just x
    | otherwise = Nothing

instance InfixHuh ScopedVariable where
  infixHuh (_ :.: x) = infixHuh x        -- ORLY?

instance InfixHuh RawVariable where
  infixHuh (Nothing, x) = infixHuh x        -- ORLY?
  infixHuh _ = Nothing


