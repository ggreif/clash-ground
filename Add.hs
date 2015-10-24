{-# LANGUAGE ViewPatterns, LambdaCase, RankNTypes, GADTs, PatternSynonyms, KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}

module Add where

-- implement the virtual machine derived in mosaic/HuttonBahr.hs

import CLaSH.Prelude
import qualified Data.List as L
import Data.Maybe

data Exp = Lit Int | Add Exp Exp deriving Show

type Eval exp = forall k . CONT k -> exp -> k

eval :: Eval Exp
eval c (\case Lit (exec c -> n') -> n'
              Add (eval (NEXT c) -> a') (a' -> b') -> b'
        -> res') = res'


data CONT k where
  ADD :: CONT k -> Int -> CONT k
  NEXT :: CONT k -> CONT (Exp -> k)
  HALT :: CONT Int

exec :: CONT k -> Int -> k
exec (ADD c a) (exec c . (a+) -> res) = res
exec (NEXT c) (eval . (ADD c) -> res) = res
exec HALT a = a


type W = Unsigned 10
type DBBits = 4
type DB = 2^DBBits
type DBI = Unsigned DB

data Instr = LOAD W  -- absolute
           | STORE W -- upmost row
           | LEFT DBI -- follow
           | RIGHT DBI
           | BRANCH DBI
           | NOP
           | STOP
           | RE
           | AD DBI


type DeBruijn = Vec DB

dbi :: Vec (DB-1) a -> DBI -> a
dbi = (!!)

redox :: DeBruijn (W, W, W) -> Instr -> (DeBruijn (W, W, W), Maybe W)
redox ((_, b, c):>(m:<l)) (LOAD addr) = (l:>(addr, b, c):>m, Nothing)
  --where blockRamPow2 (repeat 0)


redox (h:>(m:<l)) NOP = (l:>h:>m, Nothing)
redox ((a,b,c):>m) (AD (dbi m -> (d,e,f))) = ((a+d,b+e,c+f):>m, Nothing)
  --where (d,e,f) = m !! dbi
redox (h:>m) RE = (m:<h, Nothing)
redox v STOP = (v, theLook v)
  where theLook ((a, _, _):>_) = Just a

topEntity = mealy redox (repeat (0,0,0))

testInput = stimuliGenerator $ LOAD 11 :> LOAD 3 :> RE :> AD 0 :> STOP :> Nil

t = sampleN 30 $ topEntity testInput


-- ## WARM UP
data Na = Z | S Na deriving Show

pls Z = id
pls (S n) = pls n . S

-- We need something like this

pls', s', id' :: Machine a => a
pls' = convention (\arg -> untag arg ((\() -> id') :! (\n -> call pls' n {- . s' -}) :! Empy))
s' = convention(\arg -> same arg (tag 1 arg))
 where same :: a -> a -> a
       same _ = id
diag' = convention(\arg -> p arg (tag 0 (arg,arg)))
 where p :: a -> a -> a
       p _ = id
id' = convention(\arg -> arg)

class Machine a where
  -- calling convention
  convention :: Args t a => (t -> a) -> a
  tag :: Args t a => Int -> t -> a
  untag :: a -> Cases n a -> a
  call :: a -> a -> a

class Machine a => Args t a where
  nth :: t -> Int -> Maybe a
  pck :: [a] -> t

instance {-# INCOHERENT #-} Machine a => Args a a where
  nth a 0 = Just a
  nth _ _ = Nothing
  pck (a:_) = a

instance Machine a => Args () a where
  nth _ _ = Nothing
  pck _ = ()

instance Machine a => Args (a, a) a where
  nth (b, _) 0 = Just b
  nth (_, c) 1 = Just c
  nth _ _ = Nothing
  pck (b:c:_) = (b, c)


data Cases :: Na -> * -> * where
  Empy :: Cases Z a
  Case :: Args t a => (t -> a) -> Cases n a -> Cases (S n) a

pattern f :! cs = Case f cs
infixr 4 :!


data Binding where
  B :: Binding
  BConv :: Args t Binding => (t -> Binding) -> Binding
  BTag :: Args t Binding => Int -> t -> Binding

instance Show Binding where
  show B = "B"
  show (BConv f) = "CONV(B)->" L.++ show (f $ pck $ L.repeat B)
  show (BTag i t) = "TAG(" L.++ show i L.++ ":" L.++ L.intersperse ' ' (L.concat $ L.map ((show :: Binding -> String) . fromJust) $ takeWhile (not . null) $ L.map (t `nth`) [0 ..]) L.++ ")"

instance Machine Binding where
  convention = BConv
  tag = BTag
