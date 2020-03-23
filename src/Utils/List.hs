module Utils.List where

import Control.Arrow

padCat :: Eq a => [[a]] -> [[a]] -> [[a]]
padCat [] hs = hs
padCat hs [] = hs
padCat (a : as) (b : bs) = (a ++ b) : padCat as bs

data ListView a la
  = ItsNil
  | ItsCons a la
  | ItsNot

class HasListView a la where

  coalgebra :: la -> ListView a la

  listView :: la -> ([a], Maybe la)
  listView seed = case coalgebra seed of
    ItsNil       -> ([], Nothing)
    ItsCons x xs -> (x :) *** id $ listView xs
    ItsNot       -> ([], Just seed)
