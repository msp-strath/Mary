module Shonkier.Dot where

import qualified Data.Text as T
import Dot.Types

import Shonkier.Syntax
import Shonkier.Value

instance FromValue DotGraph where
  fromValue v = case v of
    VCell (VAtom "strict") b -> fromAfter2Listy (DotGraph Strict) b
    v                        -> fromAfter2Listy (DotGraph NonStrict) v

instance FromValue Directionality where
  fromValue v@(VAtom tag) = case tag of
    "graph"   -> pure Undirected
    "digraph" -> pure Directed
    _         -> Left v
  fromValue v = Left v

instance FromValue Id where
  fromValue (VString _ str) = pure (Id str)
  fromValue v = Left v

instance FromValue CardinalDirection where
  fromValue v@(VAtom tag) = case tag of
    "n"  -> pure North
    "e"  -> pure East
    "w"  -> pure West
    "s"  -> pure South
    "ne" -> pure Northeast
    "nw" -> pure Northwest
    "se" -> pure Southeast
    "sw" -> pure Southwest
    _    -> Left v
  fromValue v = Left v

instance FromValue NodeId where
  fromValue (VCell a (VCell b (VCell c VNil))) =
    NodeId <$> fromValue a
           <*> fmap Just (Port <$> fromValue b
                               <*> fmap Just (fromValue c))
  fromValue (VCell a (VCell b@(VAtom (MkAtom tag)) VNil)) = do
    a <- fromValue a
    -- hack due to https://github.com/andrewthad/dot/issues/3
    b <- fromValue b :: Either Value CardinalDirection
    pure $ NodeId a $ Just (Port (Id (T.pack tag)) Nothing)
  fromValue v = NodeId <$> fromValue v <*> pure Nothing

instance FromValue Statement where
  fromValue (VCell v@(VAtom tag) b) = case tag of
    "edge" -> StatementEdge <$> fromValue b
    "node" -> StatementNode <$> fromValue b
    _ -> Left v
  fromValue v = Left v

instance FromValue EdgeElement where
  fromValue v = EdgeNode <$> fromValue v

instance FromValue EdgeStatement where
  fromValue v = fromValue v >>= \case
    (a:b:cs) -> pure $ EdgeStatement (ListTwo a b cs) []
    _        -> Left v

instance FromValue NodeStatement where
  fromValue v@(VString _ str) = NodeStatement <$> fromValue v <*> pure []
  fromValue v = fromAfter1Listy NodeStatement v

instance FromValue Attribute where
  fromValue = fromTakes2 Attribute
