{-# LANGUAGE ViewPatterns #-}

import CLaSH.Prelude
import CLaSH.Signal.Explicit
import CLaSH.Prelude.Mealy

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
  go (_, _, over) n | n .&. over == 0 = Done
  go (mask, offs, over) n = toChopped ((n .&. mask) `shiftR` offs)
  toChopped 0 = OO
  toChopped 1 = OI
  toChopped 2 = IO
  toChopped 3 = II
  around = register' serialClock (3 :: Unsigned 24, 0, all24bits) (liftA shift2 around)
  shift2 (12582912, _, _) = (3, 0, all24bits)
  shift2 (n, offs, over) = (n `shiftL` 2, offs + 2, over `shiftL` 2)
  all24bits = 0b111111111111111111111111

topEntity :: Signal (Unsigned 24) -> Signal' SerialClock Chopped
topEntity = chop


t1 = chop $ fromList [0, 20, 0, 0]
t2 = chop' $ fromList $ cycle [0b0000010111]


-- using a mealy machine
chop' :: Signal' SerialClock (Unsigned 24) -> Signal' SerialClock (Unsigned 24)
chop' = mealy' serialClock go start where
  start = (0, 0)
  go (old, offs@10) i = ((i, 0), old .&. 0b11)
  go (old, offs) _ = ((old `shiftR` 2, offs + 2), old .&. 0b11)
