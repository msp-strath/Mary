module Data.Bwd where

data Bwd a
  = B0
  | Bwd a :< a
  deriving (Show, Functor, Foldable, Traversable)

(<>>) :: Bwd a -> [a] -> [a]
B0        <>> as = as
(az :< a) <>> as = az <>> (a : as)

(<><) :: Bwd a -> [a] -> Bwd a
az <>< []       = az
az <>< (a : as) = (az :< a) <>< as

instance Monoid (Bwd a) where
  mempty = B0
  mappend xz B0        = xz
  mappend xz (yz :< y) = mappend xz yz :< y

instance Semigroup (Bwd a) where (<>) = mappend
