{-# LANGUAGE GADTs, RankNTypes #-}

module Lepton where

import CLaSH.Prelude
import GHC.Exts
import Debug.Trace (traceShow)

class Lam f where
  lam :: ((forall s . f s a) -> f (a ': s) b) -> f s (a -> b)
  app :: f s (a -> b) -> f s a -> f s b

class Val f where
  int :: Int -> f Int

class Eval f where
  eval :: f a -> a

data Baryon s a where
  Barylam :: ((forall s . Baryon s a) -> Baryon (a ': s) b) -> Baryon s (a -> b)
  Baryapp :: Baryon s (a -> b) -> Baryon s a -> Baryon s b
  BaryInt :: Int -> Baryon s Int
  BaryVar :: a -> Baryon s a
  BaryBruijn :: CONT s' (a -> b) k -> Baryon s a

instance Lam Baryon where
  lam = Barylam
  app = Baryapp

instance Val (Baryon s) where
  int = BaryInt

instance Eval (Baryon s) where
  eval = evalB

instance Functor (Baryon s) where
  fmap f = (f <$>)

instance Applicative (Baryon s) where
  pure = BaryVar
  (<*>) = app

-- Here is our standard evaluator:
--
evalB :: Baryon s a -> a
evalB (f `Baryapp` a) = evalB f $ evalB a
evalB (BaryVar v) = v
evalB (BaryInt i) = i
evalB (Barylam f) = \x -> evalB (f (BaryVar x))
evalB (BaryBruijn (C1 a _)) = a


test :: (Lam f, Val (f '[])) => f '[] Int
test = id `app` (const `app` fortytwo `app` seven)
  where id = lam (\x->x)
        const = lam (\x->lam(\_->x))
        fortytwo = int 42
        seven = int 7

t0 :: Baryon '[] Int
t0 = test


-- derivation of the abstract machine

eval' :: CONT s a k -> Baryon s a -> k

eval' c (BaryBruijn c') | traceShow (show c, show c') False = undefined

eval' c'@(C1 _ c) (Barylam f) = eval' (CENTER c) (f (BaryBruijn c'))
  --where exec (CENTER c) b = exec c b
{- introduce CENTER for entering deeper scope -}
eval' c'@(C1 _ c) (Barylam f) = exec c (evalB (f (BaryBruijn c'))) -- for now capture the stack, later just the stack pointer!


eval' (C1 a c) (Barylam f) = exec c (evalB (f (BaryVar a))) -- this is a gamble on the form of the control stack. Does it always hold?

--eval' c (Barylam f) = exec c (\a -> evalB (f (BaryBruijn 0)))
{- can we use (DEM) ? -}
eval' c (Barylam f) = exec c (\a -> evalB (f (BaryVar a)))



eval' c (f `Baryapp` a) = eval' (C0 f c) a
{- Obi-Wan helps -}
eval' c (f `Baryapp` a) = exec (C0 f c) (eval a)
   --where exec' (C0 f c) a = exec c (eval f a)
{- help me Obi-Wan! (create C0, amend exec) -}
eval' c (f `Baryapp` a) = exec c (eval f $ eval a)
{- evalB == eval -}
eval' c (f `Baryapp` a) = exec c (evalB f $ evalB a)
eval' c (BaryVar v) = exec c v
eval' c (BaryInt i) = exec c i
{- ^^ expand evalB -}
eval' c e = exec c (eval e)  -- (OWK)


data CONT :: [*] -> * -> * -> *  where
  C0 :: Baryon s (a -> b) -> !(CONT s b k) -> CONT s a k
  C1 :: a -> !(CONT s b k) -> CONT s (a -> b) k
  CENTER :: !(CONT s b k) -> CONT (a ': s) b k
  CHALT :: CONT '[] a a

instance Show (CONT s a k) where
  show CHALT = ""
  show (C0 _ c) = '0' : show c
  show (C1 _ c) = '1' : show c
  show (CENTER c) = '^' : show c

exec :: CONT s a k -> a -> k


exec (C1 a c) f = exec c (f a) -- (DEM)


exec (C0 f c) a = eval' (C1 a c) f
{- Obi-Wan helps -}
exec (C0 f c) a = exec (C1 a c) (eval f)
  --where exec (C1 a c) f = exec c (f a) -- see above
{- help me Obi-Wan! (create C, amend exec) -}
exec (C0 f c) a = exec c (eval f a)
exec CHALT a = a

exec (CENTER c) b = exec c b


