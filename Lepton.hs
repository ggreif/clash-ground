{-# LANGUAGE GADTs #-}

module Lepton where

import CLaSH.Prelude


class Lam f where
  lam :: (f a -> f b) -> f (a -> b)
  app :: f (a -> b) -> f a -> f b

class Val f where
  int :: Int -> f Int


data Baryon a where
  Barylam :: (Baryon a -> Baryon b) -> Baryon (a -> b)
  Baryapp :: Baryon (a -> b) -> Baryon a -> Baryon b
  BaryInt :: Int -> Baryon Int
  BaryVar :: a -> Baryon a

instance Lam Baryon where
  lam = Barylam
  app = Baryapp

instance Val Baryon where
  int = BaryInt

-- Here is our standard evaluator:
--
evalB :: Baryon a -> a
evalB (f `Baryapp` a) = evalB f $ evalB a
evalB (BaryVar v) = v
evalB (BaryInt i) = i
evalB (Barylam f) = evalB . f . BaryVar


test :: (Lam f, Val f) => f Int
test = id `app` (const `app` fortytwo `app` seven)
  where id = lam (\x->x)
        const = lam (\x->lam(\_->x))
        fortytwo = int 42
        seven = int 7

t0 :: Baryon Int
t0 = test
