{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}
module Arena where

import qualified Data.List as L
import CLaSH.Prelude
import CLaSH.Prelude.BlockRam
import Data.Proxy

type BitWidth = 5

loadMeta :: SNat n -> SNat tags -> Signal (Unsigned BitWidth) -> Signal (Unsigned BitWidth)
loadMeta n tags addr = blockRamPow2 init shifted shifted read (signal 0) where
    init = 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> 13 :> 14 :> 15 :>
           0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> 13 :> 14 :> 15 :> Nil
    read = signal False
    masked = (.&. complement (shiftL 1 (fromEnum tags) - 1)) <$> addr
    shifted = flip shiftR (fromEnum n) <$> masked

topEntity = loadMeta d3 d2

testInput :: Signal (Unsigned BitWidth)
testInput = stimuliGenerator $ 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> 13 :> 14 :> 15 :> 16 :> 17 :> 18 :> 19 :> 20 :> 21 :> 22 :> 23 :> 24 :> 25 :> 26 :> 27 :> 28 :> 29 :> 30 :> 31 :> 32 :> 33 :> 34 :> Nil

instance Eq (SNat n) where
  _ == _ = True
instance Ord (SNat n) where
  _ `compare` _ = EQ
instance Num (SNat n) where
  abs = id
instance Enum (SNat n) where
  -- toEnum = fromInteger . toInteger
  fromEnum = fromInteger . toInteger
instance KnownNat n => Bounded (SNat n) where
  minBound = SNat (Proxy :: Proxy n)
  maxBound = minBound
instance Real (SNat n) where
instance Integral (SNat n) where
  toInteger (SNat p) = natVal p
