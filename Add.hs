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

data Instr = LOAD W
           | WRITE W -- upmost row
           | BRANCH DBI
           | NOP
           | STOP


type DeBruijn = Vec DB

redox :: DeBruijn (W, W, W) -> Instr -> (DeBruijn (W, W, W), Bool)
redox ((a, b, c):>(m:<l)) (LOAD addr) = (l:>(addr, b, c):>m, False)
redox v STOP = (v, True)

topEntity = mealy redox (repeat (0,0,0))

testInput = stimuliGenerator $ LOAD 11 :> LOAD 3 :> STOP :> Nil

t = sampleN 30 $ topEntity testInput
