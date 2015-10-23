{-# LANGUAGE ViewPatterns, LambdaCase, RankNTypes, GADTs, PatternSynonyms #-}

module Add where

-- implement the virtual machine derived in mosaic/HuttonBahr.hs

import CLaSH.Prelude

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
