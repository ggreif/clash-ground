{-# LANGUAGE GADTs, RankNTypes, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, ViewPatterns, AllowAmbiguousTypes #-}

module Lepton where

import CLaSH.Prelude
import GHC.Exts
import Debug.Trace (trace, traceShow, traceShowId)
import Data.Type.Equality
import qualified Data.List as L

class Lam f where
  --lam :: ((forall i . (DeBruijnIndex i (a ': s)) => f (a ': i)) -> f (b ': a ': s)) -> f ((a -> b) ': s)
  lam :: ((forall i . (TRUNC (Trunc '[] (a ': s) i) (a ': s) i) => f (a ': i)) -> f (b ': a ': s)) -> f ((a -> b) ': s)
  app :: f ((a -> b) ': s) -> f (a ': s) -> f (b ': s)

class Val f where
  int :: Int -> f (Int ': s)

class Eval f where
  eval :: f (a ': s) -> a

data Baryon s where
  --Barylam :: ((forall i . (DeBruijnIndex i (a ': s)) => Baryon (a ': i)) -> Baryon (b ': a ': s)) -> Baryon ((a -> b) ': s)
  Barylam :: ((forall i . (TRUNC (Trunc '[] (a ': s) i) (a ': s) i) => Baryon (a ': i)) -> Baryon (b ': a ': s)) -> Baryon ((a -> b) ': s)
  Baryapp :: Baryon ((a -> b) ': s) -> Baryon (a ': s) -> Baryon (b ': s)
  BaryInt :: Int -> Baryon (Int ': s)
  BaryVar :: a -> Baryon (a ': s)
  --BaryBruijn :: (DbIndex (a ': s) s' ~ idx, Consume a idx s', Builds idx) => CONT ((a -> b) ': s') k -> Baryon (a ': s)
  --BaryBruijn :: (DeBruijnIndex s (a ': s')) => CONT ((a -> b) ': s') k -> Baryon (a ': s)
  BaryBruijn :: (TRUNC idx (a ': s') s) => CONT ((a -> b) ': s') k -> Baryon (a ': s)
  BaryBruijnX :: (TRUNC idx (a ': s') s) => CONT (b ': a ': s') k -> Baryon (a ': s)

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
  show (BaryBruijn cont) = "PPP " L.++ show cont

-- Here is our standard evaluator:
--
evalB :: Baryon (a ': s) -> a
evalB (f `Baryapp` a) = evalB f $ evalB a
evalB (BaryVar v) = v
evalB (BaryInt i) = i
evalB (Barylam f) = \x -> evalB (f (BaryVar x))
--evalB (BaryBruijn (C1 a _)) = a
evalB (BaryBruijn (C1 (CENTER a _))) = a
evalB (BaryBruijnX (CENTER a _)) = a


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

-- IS THIS A BETTER WAY TO ELIMINATE (C1 (CENTER ...)) ???
eval' (C1 c) (Barylam f) | traceShow ("C1bruijn", show c) True = eval' c (f (BaryBruijnX c))

--eval' c'@(C1 c) (Barylam f) | traceShow ("C1bruijn", show c') True = eval' c (f (BaryBruijn c'))
  --where exec (CENTER c) b = exec c b
{- introduce CENTER for entering deeper scope -}
eval' c'@(C1 c) (Barylam f) = exec c (evalB (f (BaryBruijn c'))) -- for now capture the stack, later just the stack pointer!

eval' c'@(CENTER _ c) (Barylam f) | traceShow ("CENTERbruijn", show c) True = eval' c (f (BaryBruijnX c))


--eval' (C1 a c) (Barylam f) = exec c (evalB (f (BaryVar a))) -- this is a gamble on the form of the control stack. Does it always hold? -- NO: CENTER can also be (see immediately below this)


--eval' c'@(CENTER _ c''@(C1 c)) (Barylam f) | traceShow ("CENTERbruijn", show c') True = eval' (CDROP c) (f (BaryBruijn c'))

-------eval' c'@(CDROP (CENTER _ c''@(C1 c))) (Barylam f) | traceShow ("DROPbruijn", show c') True = eval' (CDROPP c) (f (BaryBruijn c'))


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

eval' c (BaryBruijn (C1 c')) | traceShow ("@@", show c', show c) True = exec c (trunc (x undefined extract c') (x undefined extract c))
  where x :: a -> (a -> b) -> a -> b
        x _ f a = f a
--eval' c@(CDROP (CENTER _ _)) (BaryBruijn c'@(CENTER _ _)) | traceShow ("@@@", show c', show c) True = exec c (trunc (extract c') (extract c))

{-
eval' c (BaryBruijn c') | traceShow ("@@", show c', show c) True = exec c (grab c' c)
  where grab :: Consume a s s' => CONT ((a -> b) ': s') k' -> CONT (a ': s) k -> a
        grab (C1 (CENTER a _)) _ = a
        grab shallow deep = peelC (extract shallow) deep
-}
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

-- AVENUE B
-- DeBruijnIndex [f, e, d, c, b, a] [c, b, a] = Consume [f, e, d, c (, ...)] 
-- :kind! DeBruijnIndex '[Float, Either Int Int, Double, Char, Bool, Int] '[Char, Bool, Int]

{-
type family DBI (odeep :: [*]) (oshallow :: [*])(dacc :: [*]) (sacc :: [*]) (deep :: [*]) (shallow :: [*]) :: Constraint where
  DBI orig oshall dacc sacc (d ': deep) (s ': shallow) = DBI orig oshall (d ': dacc) (s ': sacc) deep shallow
  DBI orig oshall dacc sacc (d ': deep) '[] = DBI orig oshall (d ': dacc) sacc deep '[]
  DBI orig oshall (d ': dacc) '[s] '[] '[] = (d ~ s, Consume d (Reverse dacc) orig, Reverse dacc ~ DbIndex oshall orig)
  DBI orig oshall (d ': dacc) (s ': sacc) '[] '[] = (d ~ s, DBI orig oshall dacc sacc '[] '[])

--type DeBruijnIndex deep shallow = DBI deep shallow '[] '[] deep shallow
class DBI deep shallow '[] '[] deep shallow => DeBruijnIndex deep shallow where
  peelX :: (shallow ~ (a ': sh)) => DB shallow -> DB deep -> a
instance DBI deep (a ': shallow) '[] '[] deep (a ': shallow) => DeBruijnIndex deep (a ': shallow) where
  peelX (TCons a _) _ = a

type family (idx ::[*]) ++ (shallow :: [*]) :: [*] where
  '[] ++ shallow = shallow
  (a ': idx) ++ shallow = a ': idx ++ shallow

class Builds (idx ::[*]) where
  rev :: DB idx

instance Builds '[] where
  rev = Lepton.Nil

instance Builds as => Builds (a ': as) where
  rev = TCons undefined rev

class Builds rev => Consume d (rev :: [*]) (deep :: [*]) where
  peel :: DB rev -> DB deep -> d
  peelC :: DB rev -> CONT (t ': deep) k -> d
  peelC db (extract -> dbd) = peel db dbd

instance Consume a '[] (a ': sha) where
  peel _ (TCons a _) = a

instance ({-(ds ++ (a ': sha)) ~ deeps, -}Consume a ds deeps) => Consume a (d ': ds) (d ': deeps) where
  peel (TCons _ rest0) (TCons _ rest) = peel rest0 rest
-}

-- AVENUE C
{-
type family INDEX (dacc :: [*]) (sacc :: [*]) (deep :: [*]) (shallow :: [*]) :: [*] where
  INDEX dacc sacc (d ': deep) (s ': shallow) = INDEX (d ': dacc) (s ': sacc) deep shallow
  INDEX dacc sacc (d ': deep) '[] = INDEX (d ': dacc) sacc deep '[]
  INDEX (d ': dacc) '[s] '[] '[] = Reverse dacc
  INDEX (d ': dacc) (s ': sacc) '[] '[] = INDEX dacc sacc '[] '[]

type DbIndex shallow deep = INDEX '[] '[] deep shallow

class Builds (DbIndex shallow deep) => LevelDiff (shallow :: [*]) (deep :: [*]) where
  type Idx shallow deep :: [*]
  type Idx shallow deep = DbIndex shallow deep
-}

--instance (shallow ~ deep) => LevelDiff shallow deep where
--  type Idx shallow deep = '[]

--instance LevelDiff shallow deep => LevelDiff shallow (d ': deep) where
--  --type Idx shallow (d ': deep) = d ': Idx shallow deep

-- AVENUE D

-- Sketch:
--    Map (==shallow, etc.) (Tails deep)  ---> [(False, shallow, deep), (False, shallow, d1), (True, sh, dn), ...]

{-
type family EqList (a :: [*]) (b :: [*]) where
  EqList a a = 'True
  EqList a b = 'False
type instance a == b = EqList a b
-}
{-
data Bool' b where
  T :: Bool' True
  F :: Bool' False

class Boolable (b :: Bool) where
 theBool :: Bool' b

instance Boolable True where theBool = T
instance Boolable False where theBool = F


class (Indexable' (shallow == deep) shallow deep) => Indexable (shallow :: [*]) (deep :: [*])
instance Indexable' (shallow == deep) shallow deep => Indexable (shallow :: [*]) (deep :: [*])

class Boolable same => Indexable' (same :: Bool) (shallow :: [*]) (deep :: [*]) where
  index :: ((a ': sh) ~ shallow) => DB shallow -> DB deep -> a

instance Indexable' True (a ': shallow) (a ': shallow) where
  index _ (TCons a _) = a


instance Indexable' (a ': shallow == deep) (a ': shallow) deep => Indexable' False (a ': shallow) (d ': deep) where
  index shallow@TCons{} (TCons _ deep) = undefined -- index shallow deep
-}

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


--class TRUNC index shallow deep => TRUNC' index shallow deep | shallow deep -> index

--instance (index ~ Trunc '[] shallow deep, TRUNC index shallow deep) => TRUNC' index shallow deep


data CONT :: [*] -> * -> * where
  C0 :: Baryon ((a -> b) ': s) -> !(CONT (b ': s) k) -> CONT (a ': s) k
  --C1 :: a -> !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  C1 :: !(CONT (b ': a ': s) k) -> CONT ((a -> b) ': s) k
  --CENTER :: !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CENTER :: a -> !(CONT (b ': s) k) -> CONT (b ': a ': s) k
  CDROP :: !(CONT (b ': a ': s) k) -> CONT (b ': x ': a ': s) k
  CDROPP :: !(CONT (b ': a ': s) k) -> CONT (b ': a ': x ': y ': s) k
  CHALT :: CONT '[a] a

instance Show (CONT (a ': s) k) where
  show CHALT = ""
  show (C0 _ c) = '0' : show c
  show (C1 (CENTER _ c)) = "(1^)" L.++ show c
  show (C1 c) = '1' : show c
  show (CENTER a c) = '^' : show c
  show (CDROP c) = '/' : show c
  show (CDROPP c) = 'X' : show c

extract :: CONT (a ': ctx) k -> DB ctx
extract (C1 (CENTER _ c)) = extract c -- these neutralize
extract CHALT = Lepton.Nil
extract (C0 _ c) = extract c
--extract (C1 (extract -> (TCons _ c))) = c
extract (CENTER a c) = TCons a $ extract c
--extract (CDROP (CENTER a c)) = TCons a (TCons (error $ show ("CDROP", c)) (extract c))
extract (CDROP (CENTER a c)) = TCons (error $ show ("CDROP", c)) (TCons a (extract c))
extract (CDROPP (CENTER a c)) = TCons a (TCons (error $ show ("CDROPP-0", c)) (TCons (error $ show ("CDROPP-1", c)) (extract c)))


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

