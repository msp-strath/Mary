module Shonkier.Pandoc where

import Data.Text (Text)
import Data.Attoparsec.Text
import Text.Pandoc.Definition

import Shonkier.Parser
import Shonkier.Syntax
import Shonkier.Value

---------------------------------------------------------------------------
-- TORAWTERM
---------------------------------------------------------------------------

instance ToRawTerm RawTerm where
  toRawTerm = id

instance ToRawTerm t => ToRawTerm [t] where
  toRawTerm = foldr (Cell . toRawTerm) Nil

instance (ToRawTerm a, ToRawTerm b) => ToRawTerm (a, b) where
  toRawTerm (a, b) = toRawTerm [toRawTerm a, toRawTerm b]

instance (ToRawTerm a, ToRawTerm b, ToRawTerm c) => ToRawTerm (a, b, c) where
  toRawTerm (a, b, c) = toRawTerm [toRawTerm a, toRawTerm b, toRawTerm c]

toListy :: ToRawTerm a => String -> [a] -> RawTerm
toListy at = Cell (Atom at) . toRawTerm

toTakes1 :: ToRawTerm a => String -> a -> RawTerm
toTakes1 at = toListy at . pure

toTakes2 :: (ToRawTerm a, ToRawTerm b) => String -> a -> b -> RawTerm
toTakes2 at a b = toListy at [toRawTerm a, toRawTerm b]

toTakes3 :: (ToRawTerm a, ToRawTerm b, ToRawTerm c)
         => String -> a -> b -> c -> RawTerm
toTakes3 at a b c = toListy at [toRawTerm a, toRawTerm b, toRawTerm c]

toAfter1Listy :: (ToRawTerm a, ToRawTerm b) => String -> a -> [b] -> RawTerm
toAfter1Listy at a b = Cell (Atom at) (Cell (toRawTerm a) (toRawTerm b))

toAfter2Listy :: (ToRawTerm a, ToRawTerm b, ToRawTerm c)
              => String -> a -> b -> [c] -> RawTerm
toAfter2Listy at a b c =
  Cell (Atom at) (Cell (toRawTerm a) (Cell (toRawTerm b) (toRawTerm c)))


instance ToRawTerm Int where
  toRawTerm = TNum . fromIntegral

instance ToRawTerm Text where
  toRawTerm = String "" []

instance ToRawTerm Format where
  toRawTerm (Format f) = toTakes1 "Format" f

instance ToRawTerm ListNumberDelim where
  toRawTerm = Atom . show

instance ToRawTerm ListNumberStyle where
  toRawTerm = Atom . show

instance ToRawTerm Block where
  toRawTerm = \case
    Null              -> Atom "Null"
    Plain ps          -> toListy "Plain" ps
    Para ps           -> toListy "Para" ps
    LineBlock ps      -> toListy "LineBlock" ps
    CodeBlock a@(b, cs, d) e
      | "mary" `elem` cs
      , Right t <- parseOnly topTerm e
        -> toAfter1Listy "Div" (a, filter ("mary" /=) cs, d) [t]
      | otherwise
        -> toTakes2 "Code" a e
    RawBlock a b      -> toTakes2 "RawBlock" a b
    BlockQuote a      -> toListy "BlockQuote" a
    OrderedList a b   -> toAfter1Listy "OrderedList" a b
    BulletList as     -> toListy "BulletList" as
    DefinitionList ds -> toListy "DefinitionList" ds
    Header a b c      -> toAfter2Listy "Header" a b c
    HorizontalRule    -> Atom "HorizontalRule"
    Div a b           -> toAfter1Listy "Div" a b

instance ToRawTerm Inline where
  toRawTerm = \case
    Str t          -> String "" [] t
    Emph is        -> toListy "Emph" is
    Strong is      -> toListy "Strong" is
    Strikeout is   -> toListy "Strikeout" is
    Superscript is -> toListy "Superscript" is
    Subscript is   -> toListy "Superscript" is
    SmallCaps is   -> toListy "SmallCaps" is
    Code a@(b, cs, d) e
      | "mary" `elem` cs
      , Right t <- parseOnly topTerm e
        -> toAfter1Listy "Span" (b, filter ("mary" /=) cs, d) [t]
      | otherwise
        -> toTakes2 "Code" a e
    SoftBreak      -> Atom "SoftBreak"
    LineBreak      -> Atom "LineBreak"
    RawInline a b  -> toTakes2 "RawInline" a b
    Link a b c     -> toTakes3 "Link" a b c
    Image a b c    -> toTakes3 "Image" a b c
    Note as        -> toListy "Note" as
    Span a b       -> toAfter1Listy "Span" a b
    Space          -> Atom "Space"

---------------------------------------------------------------------------
-- FROMVALUE
---------------------------------------------------------------------------

fromListy :: (FromValue a) => ([a] -> b) -> Value -> b
fromListy = (. fromValue)

fromAfter1Listy :: (FromValue a, FromValue b) => (a -> b -> c) -> Value -> c
fromAfter1Listy c (VCell a b) = c (fromValue a) (fromValue b)
fromAfter1Listy c _ = fromAfter1Listy c (VCell VNil VNil)

fromAfter2Listy :: (FromValue a, FromValue b, FromValue c)
                => (a -> b -> c -> d) -> Value -> d
fromAfter2Listy c (VCell a bc) = fromAfter1Listy (c (fromValue a)) bc
fromAfter2Listy c _ = fromAfter2Listy c (VCell VNil (VCell VNil VNil))

fromTakes1 :: FromValue a => (a -> b) -> Value -> b
fromTakes1 f (VCell a _) = f (fromValue a)
fromTakes1 f _ = f (fromValue VNil)

fromTakes2 :: (FromValue a, FromValue b) => (a -> b -> c) -> Value -> c
fromTakes2 f (VCell a x) = fromTakes1 (f (fromValue a)) x
fromTakes2 f v = fromTakes1 (f (fromValue VNil)) v

fromTakes3 :: (FromValue a, FromValue b, FromValue c)
       => (a -> b -> c -> d) -> Value -> d
fromTakes3 f (VCell a x) = fromTakes2 (f (fromValue a)) x
fromTakes3 f v = fromTakes2 (f (fromValue VNil)) v

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
    "Plain"          -> fromListy Plain
    "Para"           -> fromListy Para
    "LineBlock"      -> fromListy LineBlock
    "CodeBlock"      -> fromTakes2 CodeBlock
    "RawBlock"       -> fromTakes2 RawBlock
    "BlockQuote"     -> fromListy BlockQuote
    "OrderedList"    -> fromAfter1Listy OrderedList
    "BulletList"     -> fromListy BulletList
    "DefinitionList" -> fromListy DefinitionList
    "Header"         -> fromAfter2Listy Header
    "HorizontalRule" -> const HorizontalRule
    -- TODO: Table
    "Div"            -> fromAfter1Listy Div
    "Null"           -> const Null
    _                -> const Null
  fromValue _ = Null

instance FromValue Inline where
  fromValue (VString _ t) = Str t
  fromValue (VCell (VAtom tag) is) = ($ is) $ case tag of
    "Emph"        -> fromListy Emph
    "Strong"      -> fromListy Strong
    "StrikeOut"   -> fromListy Strikeout
    "Superscript" -> fromListy Superscript
    "Subscript"   -> fromListy Subscript
    "SmallCaps"   -> fromListy SmallCaps
    -- TODO: Quoted
    -- TODO: Cite
    "Code"        -> fromTakes2 Code
    "SoftBreak"   -> const SoftBreak
    "LineBreak"   -> const SoftBreak
    -- TODO: Math
    "RawInline"   -> fromTakes2 RawInline
    "Link"        -> fromTakes3 Link
    "Image"       -> fromTakes3 Image
    "Note"        -> fromListy Note
    "Span"        -> fromAfter1Listy Span
    _             -> const Space
  fromValue _ = Space
