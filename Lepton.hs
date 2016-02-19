{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, ViewPatterns #-}

module Lepton where

import CLaSH.Prelude
import GHC.Exts
import Debug.Trace (traceShow)

class Lam f where
  lam :: ((forall i . (DeBruijnIndex i (a ': s)) => f (a ': i)) -> f (b ': a ': s)) -> f ((a -> b) ': s)
  app :: f ((a -> b) ': s) -> f (a ': s) -> f (b ': s)

class Val f where
  int :: Int -> f (Int ': s)

class Eval f where
  eval :: f (a ': s) -> a

data Baryon s where
  Barylam :: ((forall i . (DeBruijnIndex i (a ': s)) => Baryon (a ': i)) -> Baryon (b ': a ': s)) -> Baryon ((a -> b) ': s)
  Baryapp :: Baryon ((a -> b) ': s) -> Baryon (a ': s) -> Baryon (b ': s)
  BaryInt :: Int -> Baryon (Int ': s)
  BaryVar :: a -> Baryon (a ': s)
  BaryBruijn :: Consume a s' s => CONT ((a -> b) ': s') k -> Baryon (a ': s)

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

test1 = (lam (\x0 -> lam (\x1 -> x1)) `app` int 2) `app` int 1

t1 :: Baryon '[Int]
t1 = test1

test2 = lam (\x0 -> lam (\_ -> lam (\x -> x0))) `app` int 2 `app` int 1 `app` int 0

t2 :: Baryon '[Int]
t2 = test2


-- derivation of the abstract machine

eval' :: CONT (a ': s) k -> Baryon (a ': s) -> k


eval' c'@(C1 c) (Barylam f) | traceShow ("C1bruijn", show c') True = eval' c (f (BaryBruijn c'))
  --where exec (CENTER c) b = exec c b
{- introduce CENTER for entering deeper scope -}
eval' c'@(C1 c) (Barylam f) = exec c (evalB (f (BaryBruijn c'))) -- for now capture the stack, later just the stack pointer!


--eval' (C1 a c) (Barylam f) = exec c (evalB (f (BaryVar a))) -- this is a gamble on the form of the control stack. Does it always hold? -- NO: CENTER can also be (see immediately below this)


eval' c'@(CENTER _ c''@(C1 c)) (Barylam f) | traceShow ("CENTERbruijn", show c') True = eval' (CDROP c) (f (BaryBruijn c''))

eval' c'@(CDROP (CENTER _ c''@(C1 c))) (Barylam f) | traceShow ("DROPbruijn", show c') True = eval' (CDROPP c) (f (BaryBruijn c''))


--eval' c (Barylam f) = exec c (\a -> evalB (f (BaryBruijn 0)))
{- can we use (DEM) ? -}
eval' c (Barylam f) | traceShow ("VAR -----> ", show c) True = exec c (\a -> evalB (f (BaryVar a)))



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
eval' c (BaryBruijn c') | traceShow ("@@", show c', show c) True = exec c (grab c' c)
  where grab :: Consume a s s' => CONT ((a -> b) ': s') k' -> CONT (a ': s) k -> a
        grab (C1 (CENTER a _)) _ = a
        grab shallow deep = peelC (extract shallow) deep
eval' c e = exec c (eval e)  -- (OWK)



type family Rev (acc :: [*]) (rdeep :: [*]) :: [*] where
  Rev acc '[] = acc
  Rev acc (a ': as) = Rev (a ': acc) as

type Reverse l = Rev '[] l

data DB :: [*] -> * where
  Nil :: DB '[]
  TCons :: t -> DB ts -> DB (t ': ts)

-- I might need such a beast later:
type family EqZip (deep :: [*]) (shallow :: [*]) :: Constraint where
  EqZip (a ': peel) '[a] = ()
  EqZip (a ': as) (a ': bs) = EqZip as bs

-- AVENUE B
-- DeBruijnIndex [f, e, d, c, b, a] [c, b, a] = Consume [f, e, d, c (, ...)] 
-- :kind! DeBruijnIndex '[Float, Either Int Int, Double, Char, Bool, Int] '[Char, Bool, Int]
type family DBI (odeep :: [*]) (dacc :: [*]) (sacc :: [*]) (deep :: [*]) (shallow :: [*]) :: Constraint where
  DBI orig dacc sacc (d ': deep) (s ': shallow) = DBI orig (d ': dacc) (s ': sacc) deep shallow
  DBI orig dacc sacc (d ': deep) '[] = DBI orig (d ': dacc) sacc deep '[]
  DBI orig (d ': dacc) '[s] '[] '[] = (d ~ s, Consume d (Reverse dacc) orig)
  DBI orig (d ': dacc) (s ': sacc) '[] '[] = (d ~ s, DBI orig dacc sacc '[] '[])

type DeBruijnIndex deep shallow = DBI deep '[] '[] deep shallow

class Consume d (rev :: [*]) (deep :: [*]) where
  peel :: DB rev -> DB deep -> d
  peelC :: DB rev -> CONT (t ': deep) k -> d
  peelC db (extract -> dbd) = peel db dbd

instance Consume a '[] (a ': deeps) where
  peel _ (TCons a _) = a

instance Consume a ds (deeps) => Consume a (d ': ds) (d ': deeps) where
  peel (TCons _ rest0) (TCons _ rest) = peel rest0 rest


data CONT :: [*] -> * -> * where
  C0 :: Baryon ((a -> b) ': s) -> !(CONT (b ': s) k) -> CONT (a ': s) k
  --C1 :: a -> !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  C1 :: !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  --CENTER :: !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CENTER :: a -> !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CDROP :: !(CONT (b ': a ': s) k) -> CONT (b ': a ': x ': s) k
  CDROPP :: !(CONT (b ': a ': s) k) -> CONT (b ': a ': x ': y ': s) k
  CHALT :: CONT '[a] a

instance Show (CONT (a ': s) k) where
  show CHALT = ""
  show (C0 _ c) = '0' : show c
  show (C1 c) = '1' : show c
  show (CENTER a c) = '^' : show c
  show (CDROP c) = '/' : show c
  show (CDROPP c) = 'X' : show c

extract :: CONT (a ': ctx) k -> DB ctx
extract CHALT = Lepton.Nil
extract (C0 _ c) = extract c
extract (C1 (extract -> (TCons _ c))) = c
extract (CENTER a c) = TCons a $ extract c
extract (CDROP (CENTER a c)) = TCons a (TCons undefined (extract c))
extract (CDROPP (CENTER a c)) = TCons a (TCons undefined (TCons undefined (extract c)))


exec :: CONT (a ': s) k -> a -> k

exec (CDROP c) f = exec c f
exec (CDROPP c) f = exec c f


exec (C1 (CENTER a c)) f = exec c (f a) -- (DEM)


exec (C0 f c) a = eval' (C1 (CENTER a c)) f
{- Obi-Wan helps -}
exec (C0 f c) a = exec (C1 (CENTER a c)) (eval f)
  --where exec (C1 a c) f = exec c (f a) -- see above
{- help me Obi-Wan! (create C, amend exec) -}
exec (C0 f c) a = exec c (eval f a)
exec CHALT a = a

exec (CENTER _ c) b = exec c b

