{-# LANGUAGE GADTs #-}

module Lepton where

import CLaSH.Prelude


class Lam f where
  lam :: (f a -> f b) -> f (a -> b)
  app :: f (a -> b) -> f a -> f b




data Baryon a where
  Barylam :: (Baryon a -> Baryon b) -> Baryon (a -> b)
  Baryapp :: Baryon (a -> b) -> Baryon a -> Baryon b
  BaryInt :: Int -> Baryon Int
  BaryVar :: a -> Baryon a

instance Lam Baryon where
  lam = Barylam
  app = Baryapp

-- Here is our standard evaluator:
--
evalB :: Baryon a -> a
evalB (f `Baryapp` a) = evalB f $ evalB a
evalB (BaryInt i) = i
evalB (Barylam f) = evalB . f . BaryVar
