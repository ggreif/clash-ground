{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, ViewPatterns, BangPatterns #-}

module Lepton where

import CLaSH.Prelude
import GHC.Exts
import Debug.Trace (trace, traceShow, traceShowId)
import Data.Type.Equality
import qualified Data.List as L

data Dict :: Constraint -> * where
  Dict :: c => Dict c


class Lam f where
  lam :: ((forall i . (TRUNC (Trunc '[] (a ': s) i) (a ': s) i) => f (a ': i)) -> f (b ': a ': s)) -> f ((a -> b) ': s)
  app :: f ((a -> b) ': s) -> f (a ': s) -> f (b ': s)

class Val f where
  int :: Int -> f (Int ': s)

class Eval f where
  eval :: f (a ': s) -> a

data Baryon s where
  --Barylam :: ((forall i . (DeBruijnIndex i (a ': s)) => Baryon (a ': i)) -> Baryon (b ': a ': s)) -> Baryon ((a -> b) ': s)
  Barylam :: ((forall i . TRUNC (Trunc '[] (a ': s) i) (a ': s) i => Baryon (a ': i)) -> Baryon (b ': a ': s)) -> Baryon ((a -> b) ': s)
  BaryPush :: ((forall i . (TRUNC (Trunc '[] (a ': x ': s) i) (a ': x ': s) i) => Baryon (a ': i)) -> Baryon (b ': a ': x ': s)) -> x -> Baryon ((a -> b) ': s)

  Baryapp :: Baryon ((a -> b) ': s) -> Baryon (a ': s) -> Baryon (b ': s)
  BaryInt :: Int -> Baryon (Int ': s)
  BaryVar :: a -> Baryon (a ': s)
  BaryBruijnX :: TRUNC idx (a ': s') s => CONT (b ': a ': s') k -> Baryon (a ': s)

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

instance Show (Baryon s) where
  show (Barylam f) = "Barylam f"
  show (Baryapp f a) = "Baryapp (" L.++show f L.++") (" L.++show a L.++")"
  show (BaryInt i) = show i
  show (BaryVar _) = "<var>"
  show (BaryBruijnX cont) = "PPX " L.++ show cont
  show (BaryPush f x) = "BaryPush f"

-- Here is our standard evaluator:
--
evalB :: Baryon (a ': s) -> a
evalB _ | True = error "don't do evalB!"
evalB (f `Baryapp` a) = evalB f $ evalB a
evalB (BaryVar v) = v
evalB (BaryInt i) = i
evalB (Barylam f) = \x -> evalB (f (BaryVar x))
evalB (BaryBruijnX (CENTER a _)) = a
evalB (BaryPush f _) = \x -> evalB (f (BaryVar x))


test :: (Lam f, Val f) => f '[Int]
test = id `app` (const `app` fortytwo `app` seven)
  where id = lam (\x->x)
        const = lam (\x->lam(\_->x))
        fortytwo = int 42
        seven = int 7

t0 :: Baryon '[Int]
t0 = test

test1 = (lam (\x0 -> lam (\x1 -> (trace "%%" x1))) `app` int 2) `app` int 1

t1 :: Baryon '[Int]
t1 = test1

test1a = lam (\x0 -> lam (\x1 -> lam (\x2 -> x2))) `app` int 3 `app` int 2 `app` int 11

t1a :: Baryon '[Int]
t1a = test1a

test2 = lam (\x0 -> lam (\_ -> lam (\x -> x0))) `app` int 2 `app` int 1 `app` int 0

t2 :: Baryon '[Int]
t2 = test2


-- derivation of the abstract machine

eval' :: CONT (a ': s) k -> Baryon (a ': s) -> k

-- IS THIS A BETTER WAY TO ELIMINATE (C1 (CENTER ...)) ???
eval' (C1 c) (Barylam f) | traceShow ("C1bruijnX", show c) True = eval' c (f (BaryBruijnX c))

--eval' c'@(C1 c) (Barylam f) | traceShow ("C1bruijn", show c') True = eval' c (f (BaryBruijn c'))
  --where exec (CENTER c) b = exec c b
{- introduce CENTER for entering deeper scope -}
--eval' c'@(C1 c) (Barylam f) = exec c (evalB (f (BaryBruijn c'))) -- for now capture the stack, later just the stack pointer!



eval' c'@(CENTER x c'') (Barylam f) | traceShow ("CENTERbruijnX!!", show c'') False = eval' c'' (BaryPush f x) --(BaryVar $ \a -> evalB (f (BaryVar a)))
-- exec c (\a -> evalB (f (BaryVar a)))

-- SCETCH: BaryPush (evalB (f (BaryVar a))) === (BaryVar $ \a -> evalB (f (BaryVar a)))
--         BaryPush' f

eval' c'@(CENTER _ c'') (Barylam f) | traceShow ("CENTERbruijnX", show c'') False = eval' ( c'') (Barylam (relevel f))
  where relevel :: ((forall (i :: [*]) . TRUNC (Trunc '[] (a ': x ': s) i) (a ': x ': s) i => Baryon (a ': i)) -> Baryon (b ': a ': x ': s))
                -> ((forall (i :: [*]) . TRUNC (Trunc '[] (a      ': s) i) (a      ': s) i => Baryon (a ': i)) -> Baryon (b ': a      ': s))
        relevel f bary = undefined --case rebase bary c' of
                           --(Dict, bla) -> bla
  --where cSTUFF :: CONT ((a -> b) ': s) k -> CONT ((a -> b) ': x ': s) k
  --      cSTUFF = CSTUFF

--So we want:

--HAVING
-- f in a shallow (a ': x ': s) and forall deep
--be transported to
-- f' in a shallow (a ': s) and forall deep






{-
eval' c'@(CENTER a c'') (Barylam f) | traceShow ("CENTERbruijnX", show c') True = eval' c (f (BaryBruijnX c'))
  where c = _ (BaryBruijnX c') c''
        foo :: CONT ((a2 -> b) ': s) k -> CONT (b ': a2 ': a ': s) k
        foo = undefined -- CDROPX
-}

eval' c' (Barylam f) | traceShow ("bruijnX", show c') True = eval' c (f (BaryBruijnX c))
  where c = CDROPX c'
        --foo :: CONT ((a -> b) : s) k -> CONT (b : a : s) k
        --foo = undefined

--eval' (C1 a c) (Barylam f) = exec c (evalB (f (BaryVar a))) -- this is a gamble on the form of the control stack. Does it always hold? -- NO: CENTER can also be (see immediately below this)


--eval' c'@(CENTER _ c''@(C1 c)) (Barylam f) | traceShow ("CENTERbruijn", show c') True = eval' (CDROP c) (f (BaryBruijn c'))

-------eval' c'@(CDROP (CENTER _ c''@(C1 c))) (Barylam f) | traceShow ("DROPbruijn", show c') True = eval' (CDROPP c) (f (BaryBruijn c'))


--eval' c (Barylam f) = exec c (\a -> evalB (f (BaryBruijn 0)))
{- can we use (DEM) ? -}
eval' c (Barylam f) | traceShow ("VAR -----> ", show c) True = exec c (\a -> evalB (f (BaryVar a)))



--- BARYPUSH

eval' c@(C1 c''@(CENTER a c')) p@(BaryPush f x) | traceShow ("PUSH -----> ", show c) True = eval' (c2 ) (f (BaryBruijnX (CENTER a (CENTER x c'))))
  where c2 = (CENTER a (CENTER x c'))

--eval' c@(C1 (CENTER a c')) p@(BaryPush f _) | traceShow ("PUSH -----> ", show c) True = eval' (C2 c') (f (BaryVar a))

--eval' c@(C1 (CENTER a c')) (BaryPush f) | traceShow ("PUSH -----> ", show c) True = exec c' (evalB (f (eval' $ C1 (CENTER a CHALT) )))
eval' c (BaryPush f _) | traceShow ("PUSH VAR -----> ", show c) True = exec c (\a -> evalB (f (BaryVar a)))



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


eval' c (BaryBruijnX c') | traceShow ("@@X", show c', show c) True = exec c (trunc (extract c') (extract c))


eval' c e = exec c (eval (traceShow ("EVAL:::", c, e) e))  -- (OWK)



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
  --rebase :: TRUNC (Trunc '[] (a2 : a1 : s1) i) (a2 : a1 : s1) i => Dict (TRUNC (Trunc '[] (a2 : s1) i) (a2 : s1) i)
  --rebase :: (shallow ~ (a ': x ': s)) => DB shallow -> DB deep -> Dict (TRUNC (Trunc '[] (a : s) deep) (a : s) deep)
  rebase :: (shallow ~ (a ': x ': s)) => Baryon (a ': s) -> CONT deep k -> (Dict (TRUNC (Trunc '[] (a ': s) deep) (a ': s) deep), Baryon (a ': s))

  shorten :: (shallow ~ (a2 ': a1 ': s1)) => (Baryon (a2 ': deep)
                  -> Baryon (b1 ': a2 ': a1 ': s1))
                 -> (forall (i :: [*]).
                     TRUNC (Trunc '[] (a2 ': s1) i) (a2 ': s1) i =>
                     Baryon (a2 ': i))
                 -> Baryon (b1 ': a2 ': s1)

instance ('[] ~ Trunc '[] (a ': shallow) deep, (a ': shallow) ~ deep) => TRUNC '[] (a ': shallow) deep where
  trunc _ (TCons a _) = trace "DONE" $ a

instance ((i ': indx) ~ Trunc '[] (a ': shallow) (d ': deep), TRUNC indx (a ': shallow) deep) => TRUNC (i ': indx) (a ': shallow) (d ': deep) where
  trunc sh (TCons _ rest) = trace "DRILLING" $ trunc sh rest



data CONT :: [*] -> * -> * where
  C0 :: Baryon ((a -> b) ': s) -> !(CONT (b ': s) k) -> CONT (a ': s) k
  --C1 :: a -> !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  C1 :: !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  --CENTER :: !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CENTER :: a -> !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CDROPX :: !(CONT ((a' -> b) ': s) k) -> CONT (b ': a ': s) k

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
--extract (C1 (extract -> (TCons _ c))) = c
extract (CENTER a c) = TCons a $ extract c
extract (CDROPX c) = TCons (error $ show ("CDROPX", c)) $ extract c




exec :: CONT (a ': s) k -> a -> k

exec (CDROPX c) !f = exec c (const f)


exec (C1 (CENTER a c)) !f = exec c (f a) -- (DEM)


exec (C0 f c) !a = eval' (C1 (CENTER a c)) f
{- Obi-Wan helps -}
exec (C0 f c) !a = exec (C1 (CENTER a c)) (eval f)
  --where exec (C1 a c) f = exec c (f a) -- see above
{- help me Obi-Wan! (create C, amend exec) -}
exec (C0 f c) !a = exec c (eval f a)
exec CHALT !a = a

exec (CENTER _ c) !b = exec c b

