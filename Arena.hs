{-# LANGUAGE PolyKinds, ScopedTypeVariables #-}
module Arena where

import qualified Data.List as L
import CLaSH.Prelude
import CLaSH.Prelude.BlockRam
import Data.Proxy

type BitWidth = 5

load :: (KnownNat n, KnownNat tags) => SNat n -> SNat tags -> Signal (Unsigned BitWidth) -> Signal (Unsigned BitWidth)
load _ _ addr = blockRamPow2 init addr addr read (signal 0) where
    init = 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> 13 :> 14 :> 15 :>
           0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> 13 :> 14 :> 15 :> Nil
    read = signal False
    -- mask 

topEntity = load d3 d1

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
