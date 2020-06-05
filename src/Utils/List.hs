{-# LANGUAGE ConstraintKinds #-}

module Utils.List where

import Control.Arrow

padCat :: Eq a => [[a]] -> [[a]] -> [[a]]
padCat [] hs = hs
padCat hs [] = hs
padCat (a : as) (b : bs) = (a ++ b) : padCat as bs

mayZipWith :: (a -> b -> Maybe c) -> [a] -> [b] -> Maybe [c]
mayZipWith f []       []       = pure []
mayZipWith f (a : as) (b : bs) =
  (:) <$> f a b <*> mayZipWith f as bs
mayZipWith _ _ _ = Nothing

data ListView a la
  = ItsNil
  | ItsCons a la
  | ItsNot

class HasListView a la where

  coalgebra :: la -> ListView a la

  listView :: la -> ([a], Maybe la)
  listView seed = case coalgebra seed of
    ItsNil       -> ([], Nothing)
    ItsCons x xs -> first (x :) $ listView xs
    ItsNot       -> ([], Just seed)

type SelfListView la = HasListView la la

longestCommonPrefix :: Eq a => [[a]] -> [a]
longestCommonPrefix [] = []
longestCommonPrefix lists = foldr1 commonPrefix lists
  where
    commonPrefix :: Eq a => [a] -> [a] -> [a]
    commonPrefix (x:xs) (y:ys) | x == y = x : commonPrefix xs ys
    commonPrefix _ _ = []
