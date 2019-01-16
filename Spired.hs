{-# language GADTs, TypeFamilies #-}


import CLaSH.Prelude


-- let's define a bunch of addressable execution units
-- which perform primitives and lambda application

-- use tagged sums, tag should be included in primitive's
-- "opcode"

-- Integers (tags)
-- - 0 unevaled
-- - 1 = 0
-- - 2 = 1
-- - 3 = S16-bit

-- corresponding "add" primitives:
-- "zero" (add/1/1)
-- "one" (add/2/1, add/1/2)
-- "two" (add/2/2)
-- "id" (add/3/1, add/1/3)
-- "inc" (add/3/2, add/2/3)
-- add/3/3

data PrimitiveOp = Add | Lam

-- Execution units: "ID": or 0 x == add 0 x


data family Tagged :: Nat -> *

data instance Tagged 2 where
  --T0 -- thunk
  T1 :: Unsigned 16 -> Tagged 2
  T2 :: Unsigned 16 -> Tagged 2
  T3 :: Unsigned 16 -> Tagged 2

-- char, integral or pointer
data Immediate = C | I (Tagged 2) | P (Tagged 2)

inc (I T1{}) = I (T2 0)
inc (I T2{}) = I (T3 3)
inc (I (T3 n)) = I (T3 $ n + 1)

add (I (T3 m)) (I (T3 n)) = I (T2 $ m + n)


-- expand lambdas on the fly


