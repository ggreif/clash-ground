module Lepton where

import CLaSH.Prelude


class Lam f where
  lam :: (f -> f) -> f
  app :: f -> f -> f




data Baryon a = Barylam (Baryon a -> Baryon a) | Baryapp (Baryon a) (Baryon a) | BaryInt Int

instance Lam (Baryon a) where
  lam = Barylam
  app = Baryapp




evalB :: (a~Int) => Baryon a -> a
evalB (_ `Baryapp` _) = 34

