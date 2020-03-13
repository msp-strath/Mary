module Mary.Pandoc where

import Control.Monad.Writer (Writer, runWriter, tell)
import Control.Newtype

import Data.Attoparsec.Text
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Text.Pandoc.Builder
import Text.Pandoc.Walk

import Shonkier.Parser
import Shonkier.Semantics
import Shonkier.ShonkierJS

process :: Pandoc -> IO Pandoc
process doc0 = return doc3 where
  (doc1, defns) = runWriter (walkM snarfMaryDef doc0)
  ps = concat [p | ds <- defns, let Right p = parseOnly program ds]
  env = mkGlobalEnv ps
  doc2 = walk (evalMary env) doc1

  h1 :: Block -> Maybe Inlines
  h1 (Header 1 _ is) = Just (fromList is)
  h1 _ = Nothing

  doc3 = setTitle (fromMaybe "Title TBA" (ala' First query h1 doc0))
       . setMeta "jsGlobalEnv" (jsGlobalEnv ps)
       $ doc2

snarfMaryDef :: Block -> Writer [Text] Block
snarfMaryDef c@(CodeBlock (_, cs, _) p)
  | elem "mary-def" cs
  = if "keep" `elem` cs
    then c <$ tell [p]
    else Null <$ tell [p]
snarfMaryDef b = return b

evalMary :: Env -> Block -> Block
evalMary env (CodeBlock (_, cs, _) e) | elem "mary" cs =
  case parseOnly (term <* endOfInput) e of
    Left _ -> Null
    Right t -> case shonkier env t of
      Value v -> fromValue v
      _ -> Null
evalMary _ b = b

unary :: (FromValue a) => (a -> b) -> Value -> b
unary = (. fromValue)

binary :: (FromValue a, FromValue b) => (a -> b -> c) -> Value -> c
binary c (VCell a b) = c (fromValue a) (fromValue b)
binary c _ = binary c (VCell VNil VNil)

ternary :: (FromValue a, FromValue b, FromValue c)
        => (a -> b -> c -> d) -> Value -> d
ternary c (VCell a bc) = binary (c (fromValue a)) bc
ternary c _ = ternary c (VCell VNil (VCell VNil VNil))

instance FromValue Int where
  fromValue (VNum d) = truncate d
  fromValue _ = 0

instance FromValue Text where
  fromValue (VString _ s) = s
  fromValue _ = ""

instance FromValue Format where
  fromValue = unary Format

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
    "Plain"          -> unary Plain
    "Para"           -> unary Para
    "LineBlock"      -> unary LineBlock
    "CodeBlock"      -> binary CodeBlock
    "RawBlock"       -> binary RawBlock
    "BlockQuote"     -> unary BlockQuote
    "OrderedList"    -> binary OrderedList
    "BulletList"     -> unary BulletList
    "DefinitionList" -> unary DefinitionList
    "Header"         -> ternary Header
    "HorizontalRule" -> const HorizontalRule
    -- TODO: Table
    "Div"            -> binary Div
    "Null"           -> const Null
    _                -> const Null
  fromValue _ = Null

instance FromValue Inline where
  fromValue (VString _ t) = Str t
  fromValue _ = Space
