{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

module Lepton where

import CLaSH.Prelude
import GHC.Exts
import Debug.Trace (traceShow)

class Lam f where
  lam :: ((forall i . (b ': a ': s `Suffixed` a ': i) => f (a ': i)) -> f (b ': a ': s)) -> f ((a -> b) ': s)
  app :: f ((a -> b) ': s) -> f (a ': s) -> f (b ': s)

class Val f where
  int :: Int -> f (Int ': s)

class Eval f where
  eval :: f (a ': s) -> a

data Baryon s where
  Barylam :: ((forall i . (b ': a ': s `Suffixed` a ': i) => Baryon (a ': i)) -> Baryon (b ': a ': s)) -> Baryon ((a -> b) ': s)
  Baryapp :: Baryon ((a -> b) ': s) -> Baryon (a ': s) -> Baryon (b ': s)
  BaryInt :: Int -> Baryon (Int ': s)
  BaryVar :: a -> Baryon (a ': s)
  BaryBruijn :: CONT ((a -> b) ': s') k -> Baryon (a ': s)

instance Lam Baryon where
  lam = Barylam
  app = Baryapp

instance Val Baryon where
  int = BaryInt

instance Eval Baryon where
  eval = evalB

{-
instance Functor (Baryon s) where
  fmap f = (f <$>)

instance Applicative (Baryon s) where
  pure = BaryVar
  (<*>) = app
-}

-- Here is our standard evaluator:
--
evalB :: Baryon (a ': s) -> a
evalB (f `Baryapp` a) = evalB f $ evalB a
evalB (BaryVar v) = v
evalB (BaryInt i) = i
evalB (Barylam f) = \x -> evalB (f (BaryVar x))
--evalB (BaryBruijn (C1 a _)) = a
evalB (BaryBruijn (C1 (CENTER a _))) = a


test :: (Lam f, Val f) => f '[Int]
test = id `app` (const `app` fortytwo `app` seven)
  where id = lam (\x->x)
        const = lam (\x->lam(\_->x))
        fortytwo = int 42
        seven = int 7

t0 :: Baryon '[Int]
t0 = test

{-
-- derivation of the abstract machine

eval' :: CONT s a k -> Baryon s a -> k


eval' c'@(C1 _ c) (Barylam f) = eval' (c) (f (BaryBruijn c'))
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
eval' c (BaryBruijn c') | traceShow (show c', show c) True = exec c (grab c' c)
  where grab :: CONT s' (a -> b) k' -> CONT s a k -> a
        grab (C1 a _) _ = a
eval' c e = exec c (eval e)  -- (OWK)

type Every = forall a . a
-}

--revGrab :: CONT s' a' k' -> CONT s a k -> RevGrab s' (Rev '[] s)
--revGrab (C1 a CHALT) (CENTER CHALT) = undefined -- a

type family RevGrab (shallow :: [*]) (rdeep :: [*]) :: * where
  RevGrab '[] (a ': as) = a
  RevGrab (s ': ss) (a ': as) = RevGrab ss as

type family Rev (acc :: [*]) (rdeep :: [*]) :: [*] where
  Rev acc '[x] = acc
  --Rev acc ((a -> b) ': as) = Rev (b ': a ': acc) as
  Rev acc (a ': as) = Rev (a ': acc) as

data DB :: [*] -> * where
  Nil :: DB '[]
  TCons :: t -> DB ts -> DB (t ': ts)

rev :: DB acc -> CONT s k -> DB (Rev acc s)
rev acc CHALT = acc
--rev acc (C1 a (CENTER CHALT)) = rev (TCons a acc) CHALT -- TODO!

infix 4 `Suffixed`
class deep `Suffixed` shallow where
  grab :: (shallow ~ (a ': rest)) => CONT deep k -> CONT shallow k -> a

instance '[b, a] `Suffixed` '[a] where
  grab (CENTER a CHALT) (C1 _) = a

instance (b ': d ': deep `Suffixed` shallow) => (b ': a ': d ': deep) `Suffixed` shallow where
  grab (CENTER _ c) = grab c

--instance (b ': a ': deep `Suffixed` shallow) => ((a -> b) ': deep) `Suffixed` shallow where
--  grab (C1 c) = grab c

--instance (b ': a ': deep `Suffixed` shallow) => (b ': deep) `Suffixed` shallow where


data CONT :: [*] -> * -> *  where
  C0 :: Baryon ((a -> b) ': s) -> !(CONT (b ': s) k) -> CONT (a ': s) k
  --C1 :: a -> !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  C1 :: !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  --CENTER :: !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CENTER :: a -> !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CHALT :: CONT '[a] a

instance Show (CONT (a ': s) k) where
  show CHALT = ""
  show (C0 _ c) = '0' : show c
  show (C1 c) = '1' : show c
  show (CENTER a c) = '^' : show c
{-

exec :: CONT s a k -> a -> k


exec (C1 a c) f = exec c (f a) -- (DEM)


exec (C0 f c) a = eval' (C1 a (CENTER c)) f
{- Obi-Wan helps -}
exec (C0 f c) a = exec (C1 a (CENTER c)) (eval f)
  --where exec (C1 a c) f = exec c (f a) -- see above
{- help me Obi-Wan! (create C, amend exec) -}
exec (C0 f c) a = exec c (eval f a)
exec CHALT a = a

exec (CENTER c) b = exec c b

-}
