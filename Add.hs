{-# LANGUAGE ViewPatterns, LambdaCase, RankNTypes, GADTs #-}

module Add where

-- implement the virtual machine derived in mosaic/HuttonBahr.hs

import CLaSH.Prelude

data Exp = Lit Int | Add Exp Exp deriving Show

type Eval exp = forall k . CONT k -> exp -> k

eval'' :: Eval Exp
eval'' c (\case Lit (exec c -> n') -> n'
                Add (eval'' (C1 c) -> a') (a' -> b') -> b'
          -> res') = res'


data CONT k where
  C0 :: CONT k -> Int -> CONT k   -- ADD
  C1 :: CONT k -> CONT (Exp -> k) -- NEXT
  C2 :: CONT Int                  -- HALT

exec :: CONT k -> Int -> k
exec (C0 c a) (exec c . (a+) -> res) = res
exec (C1 c) (eval'' . (C0 c) -> res) = res
exec C2 a = a
