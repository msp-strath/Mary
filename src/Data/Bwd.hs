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
  mappend = (<>)

instance Semigroup (Bwd a) where
  xz <> B0        = xz
  xz <> (yz :< y) = (xz <> yz) :< y
