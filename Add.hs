{-# LANGUAGE ViewPatterns, LambdaCase, RankNTypes, GADTs #-}

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
