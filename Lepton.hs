{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, MultiParamTypeClasses, ViewPatterns, BangPatterns #-}

module Lepton where

import CLaSH.Prelude
import GHC.Exts
import Debug.Trace (trace, traceShow, traceShowId)
import Data.Type.Equality
import qualified Data.List as L


class Lam (f :: [*] -> * -> *) where
  lam :: ((forall i . (TRUNC (Trunc '[] (a ': s) i) (a ': s) i) => f i a) -> f (a ': s) b) -> f s (a -> b)
  app :: f s (a -> b) -> f s a -> f s b

class Val (f :: [*] -> * -> *) where
  int :: Int -> f s Int

class Eval (f :: [*] -> * -> *) where
  eval :: f s a -> a

data Baryon ctx s where
  Barylam :: ((forall i . TRUNC (Trunc '[] (a ': s) i) (a ': s) i => Baryon i a) -> Baryon (a ': s) b) -> Baryon s (a -> b)
  Baryapp :: Baryon s (a -> b) -> Baryon s a -> Baryon s b

  BaryInt :: Int -> Baryon s Int
  BaryVar :: a -> Baryon s a
  BaryBruijn :: TRUNC idx (a ': s') s => CONT (b ': a ': s') k -> Baryon s a

instance Lam Baryon where
  lam = Barylam
  app = Baryapp

instance Val Baryon where
  int = BaryInt

instance Eval Baryon where
  eval = evalB


instance Functor (Baryon s) where
  fmap f = (f <$>)

instance Applicative (Baryon s) where
  pure = BaryVar
  (<*>) = app


instance Show (Baryon ctx s) where
  show (Barylam f) = "Barylam f"
  show (Baryapp f a) = "Baryapp (" L.++show f L.++") (" L.++show a L.++")"
  show (BaryInt i) = show i
  show (BaryVar _) = "<var>"
  show (BaryBruijn cont) = "PPX " L.++ show cont

-- Here is our standard evaluator:
--
evalB :: Baryon s a -> a
evalB _ | True = error "don't do evalB!"
evalB (f `Baryapp` a) = evalB f $ evalB a
evalB (BaryVar v) = v
evalB (BaryInt i) = i
evalB (Barylam f) = \x -> evalB (f (BaryVar x))
evalB (BaryBruijn _) = error "Cannot look up De Bruijn index from evalB"


test :: (Lam f, Val f) => f '[] Int
test = id `app` (const `app` fortytwo `app` seven)
  where id = lam (\x->x)
        const = lam (\x->lam(\_->x))
        fortytwo = int 42
        seven = int 7

t0 :: Baryon '[] Int
t0 = test

test1 = (lam (\x0 -> lam (\x1 -> (trace "%%" x1))) `app` int 2) `app` int 1

t1 :: Baryon '[] Int
t1 = test1

test1a = lam (\x0 -> lam (\x1 -> lam (\x2 -> x2))) `app` int 3 `app` int 2 `app` int 11

t1a :: Baryon '[] Int
t1a = test1a

test1b = lam (\x0 -> lam (\x1 -> lam (\x2 -> lam (\x3 -> x3)))) `app` int 4 `app` int 3 `app` int 2 `app` int 111

t1b :: Baryon '[] Int
t1b = test1b


test2 = lam (\x0 -> lam (\_ -> lam (\x -> x0))) `app` int 2 `app` int 1 `app` int 0

t2 :: Baryon '[] Int
t2 = test2


-- derivation of the abstract machine

eval' :: CONT (a ': s) k -> Baryon s a -> k

-- IS THIS A BETTER WAY TO ELIMINATE (C1 (CENTER ...)) ???
eval' c'@(C1 c) (Barylam f) | traceShow ("C1bruijnX", show c') True = eval' c (f (BaryBruijn c))

--eval' c'@(C1 c) (Barylam f) | traceShow ("C1bruijn", show c') True = eval' c (f (BaryBruijn c'))
  --where exec (CENTER c) b = exec c b
{- introduce CENTER for entering deeper scope -}
--eval' c'@(C1 c) (Barylam f) = exec c (evalB (f (BaryBruijn c'))) -- for now capture the stack, later just the stack pointer!

eval' c@(CENTER x (C1 (CENTER a c'))) (Barylam f) | traceShow ("CENTERbruijnX", show c) True = eval' c2 (f (BaryBruijn c2))
  where c2 = CENTER a (CENTER x c') -- bubble x down

eval' c@(CENTER y (CENTER x (C1 (CENTER a c')))) (Barylam f) | traceShow ("CENTERbruijnX!!", show c) True = eval' c2 (f (BaryBruijn c2))
  where c2 = CENTER a (CENTER y (CENTER x c')) -- bubble x, y down

eval' c@(CENTER z (CENTER y (CENTER x (C1 (CENTER a c'))))) (Barylam f) | traceShow ("CENTERbruijnX!!!", show c) True = eval' c2 (f (BaryBruijn c2))
  where c2 = CENTER a (CENTER z (CENTER y (CENTER x c'))) -- bubble x, y, z down


eval' c' (Barylam f) | traceShow ("bruijnX", show c') True = eval' c (f (BaryBruijn c))
  where c = CDROPX c' -- EVIL! see above cases how to eliminate CDROPX


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


eval' c (BaryBruijn c') | traceShow ("@@X", show c', show c) True = exec c (trunc (extract c') (extract c))


eval' c e = exec c (eval (traceShow ("EVAL:::", c, e) e))  -- (OWK)


data DB :: [*] -> * where
  Nil :: DB '[]
  TCons :: t -> DB ts -> DB (t ': ts)

-- all other avenues, see git history

-- AVENUE E

-- :kind! Trunc '[] '[Char, Bool, Int] '[Float, Either Int Int, Double, Char, Bool, Int]
-- :kind! Trunc '[] '[Float, Either Int Int, Double, Char, Bool, Int] '[Float, Either Int Int, Double, Char, Bool, Int]

-- trunc (TCons 'k' Lepton.Nil) (TCons 7 $ TCons 5 $ TCons 'l' Lepton.Nil)
-- --> 'l'

type family Trunc (acc :: [*]) (shallow :: [*]) (deep :: [*]) :: [*] where
  Trunc acc sh sh = acc
  Trunc acc sh (d ': deep) = d ': Trunc acc sh deep

class (index ~ Trunc '[] shallow deep) => TRUNC index shallow deep where
  trunc :: (shallow ~ (a ': sh)) => DB shallow -> DB deep -> a

instance ('[] ~ Trunc '[] (a ': shallow) deep, (a ': shallow) ~ deep) => TRUNC '[] (a ': shallow) deep where
  trunc _ (TCons a _) = trace "DONE" $ a

instance ((i ': indx) ~ Trunc '[] (a ': shallow) (d ': deep), TRUNC indx (a ': shallow) deep) => TRUNC (i ': indx) (a ': shallow) (d ': deep) where
  trunc sh (TCons _ rest) = trace "DRILLING" $ trunc sh rest



data CONT :: [*] -> * -> * where
  C0 :: Baryon s (a -> b) -> !(CONT (b ': s) k) -> CONT (a ': s) k
  C1 :: !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  CENTER :: a -> !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CDROPX :: !(CONT ((a' -> b) ': s) k) -> CONT (b ': a ': s) k -- THIS NEEDS TO BE ELIMINATED!
  CHALT :: CONT '[a] a

instance Show (CONT (a ': s) k) where
  show CHALT = ""
  show (C0 _ c) = '0' : show c
  show (C1 (CENTER _ c)) = "(1^)" L.++ show c
  show (C1 c) = '1' : show c
  show (CENTER a c) = '^' : show c
  show (CDROPX c) = 'X' : show c

extract :: CONT (a ': ctx) k -> DB ctx
extract (C1 (CENTER _ c)) = extract c -- these neutralize
extract CHALT = Lepton.Nil
extract (C0 _ c) = extract c
extract (CENTER a c) = TCons a $ extract c
extract (CDROPX c) = TCons (error $ show ("CDROPX", c)) $ extract c


exec :: CONT (a ': s) k -> a -> k

exec (CDROPX c) !f = exec c (const f)

exec (C1 (CENTER a c)) !f = exec c (f a)

exec (C0 f c) !a = eval' (C1 (CENTER a c)) f
{- Obi-Wan helps -}
exec (C0 f c) !a = exec (C1 (CENTER a c)) (eval f)
  --where exec (C1 a c) f = exec c (f a) -- see above
{- help me Obi-Wan! (create C, amend exec) -}
exec (C0 f c) !a = exec c (eval f a)
exec CHALT !a = a

exec (CENTER _ c) !b = exec c b

