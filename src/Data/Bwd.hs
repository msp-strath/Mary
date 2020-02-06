{-# LANGUAGE DeriveTraversable #-}

module Data.Bwd where

data Bwd a
  = Nil
  | Bwd a :< a
  deriving (Show, Functor, Foldable, Traversable)

(<>>) :: Bwd a -> [a] -> [a]
Nil       <>> as = as
(az :< a) <>> as = az <>> (a : as)

