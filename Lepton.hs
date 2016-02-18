{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses #-}

module Lepton where

import CLaSH.Prelude
import GHC.Exts
import Debug.Trace (traceShow)

class Lam f where
  lam :: ((forall i . (i `Suffixed` a ': s, DeBruijnIndex i (a ': s)) => f (a ': i)) -> f (b ': a ': s)) -> f ((a -> b) ': s)
  app :: f ((a -> b) ': s) -> f (a ': s) -> f (b ': s)

class Val f where
  int :: Int -> f (Int ': s)

class Eval f where
  eval :: f (a ': s) -> a

data Baryon s where
  Barylam :: ((forall i . (i `Suffixed` a ': s, DeBruijnIndex i (a ': s)) => Baryon (a ': i)) -> Baryon (b ': a ': s)) -> Baryon ((a -> b) ': s)
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
  Rev acc '[] = acc
  Rev acc (a ': as) = Rev (a ': acc) as

type Reverse l = Rev '[] l

data DB :: [*] -> * where
  Nil :: DB '[]
  TCons :: t -> DB ts -> DB (t ': ts)

--rev :: DB acc -> CONT s k -> DB (Rev acc s)
--rev acc CHALT = acc
--rev acc (C1 a (CENTER CHALT)) = rev (TCons a acc) CHALT -- TODO!

type family EqZip (deep :: [*]) (shallow :: [*]) :: Constraint where
  EqZip (a ': peel) '[a] = ()
  EqZip (a ': as) (a ': bs) = EqZip as bs

infix 4 `Suffixed`
class (Reverse deep `EqZip` Reverse shallow) => deep `Suffixed` shallow where
  grab :: (shallow ~ (a ': rest)) => CONT deep k -> CONT shallow k -> a

instance '[a] `Suffixed` '[a] where
  --grab (CENTER a CHALT) (C1 _) = a

-- both grow the same way
--instance (d ': deep `Suffixed` shallow) => (a ': d ': deep) `Suffixed` (a ': shallow) where

-- deep deepens, shallow remains
instance (Reverse (a ': d ': deep) `EqZip` Reverse shallow, d ': deep `Suffixed` shallow) => (a ': d ': deep) `Suffixed` shallow where
  --grab (CENTER _ c) = grab c

-- AVENUE B
-- DeBruijnIndex [f, e, d, c, b, a] [c, b, a] = Consume [f, e, d, c (, ...)] 
-- :kind! DeBruijnIndex '[Float, Either Int Int, Double, Char, Bool, Int] '[Char, Bool, Int]
type family DBI (odeep :: [*]) (dacc :: [*]) (sacc :: [*]) (deep :: [*]) (shallow :: [*]) :: Constraint where
  DBI orig dacc sacc (d ': deep) (s ': shallow) = DBI orig (d ': dacc) (s ': sacc) deep shallow
  DBI orig dacc sacc (d ': deep) '[] = DBI orig (d ': dacc) sacc deep '[]
  DBI orig (d ': dacc) '[s] '[] '[] = (d ~ s, Consume d (Reverse (d ': dacc)) orig)
  DBI orig (d ': dacc) (s ': sacc) '[] '[] = (d ~ s, DBI orig dacc sacc '[] '[])

type DeBruijnIndex deep shallow = DBI deep '[] '[] deep shallow

class Consume d (rev :: [*]) (deep :: [*]) where
  -- pll :: DB deep -> d
instance Consume Int '[Int] '[Int]
instance Consume Int '[Int, Int] '[Int, Int]


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
