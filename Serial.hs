{-# LANGUAGE ViewPatterns #-}

import CLaSH.Prelude
import CLaSH.Signal.Explicit

type SerialClock = Clk "system" 100
serialClock :: SClock SerialClock
serialClock = sclock

oversampling a b = register' serialClock a . unsafeSynchronizer systemClock serialClock
                 . register' systemClock b


data Chopped = OO | OI | IO | II | Done
  deriving (Show, Read, Bounded, Eq, Ord, Enum)

chop :: Signal (Unsigned 24) -> Signal' SerialClock Chopped
chop = (liftA2 go around) . oversampling 0 0 where
  go _ 0 = Done
  go (mask, offs) n = toChopped ((n .&. mask) `shiftR` offs)
  around = register' serialClock (3 :: Unsigned 24, 0) (liftA shift2 around)
  toChopped 0 = OO
  toChopped 1 = OI
  toChopped 2 = IO
  toChopped 3 = II
  shift2 (12582912, _) = (3, 0)
  shift2 (n, offs) = (n `shiftL` 2, offs + 2)

topEntity :: Signal (Unsigned 24) -> Signal' SerialClock Chopped
topEntity = chop


t1 = chop $ fromList [0, 20, 0, 0]