module Data.Bwd where

import Data.Semigroup (Semigroup, (<>)) -- needed for ghc versions <= 8.2.2

data Bwd a
  = Nil
  | Bwd a :< a
  deriving (Show, Functor, Foldable, Traversable)

(<>>) :: Bwd a -> [a] -> [a]
Nil       <>> as = as
(az :< a) <>> as = az <>> (a : as)

(<><) :: Bwd a -> [a] -> Bwd a
az <>< []       = az
az <>< (a : as) = (az :< a) <>< as

instance Monoid (Bwd a) where
  mempty = Nil
  mappend xz Nil       = xz
  mappend xz (yz :< y) = mappend xz yz :< y

instance Semigroup (Bwd a) where (<>) = mappend
