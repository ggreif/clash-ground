{-# LANGUAGE ViewPatterns #-}

import CLaSH.Prelude
import CLaSH.Signal.Explicit

type SerialClock = Clk "system" 100
serialClock :: SClock SerialClock
serialClock = sclock

oversampling a b = register' serialClock a . unsafeSynchronizer systemClock serialClock
                 . register' systemClock b


data Chopped = O | I | Done
  deriving (Show, Read, Bounded, Eq, Ord, Enum)

chop :: Signal (Unsigned 24) -> Signal' SerialClock Chopped
chop = oversampling Done Done . (liftA go) where
  go 0 = Done
  go n = toChopped (n .&. 0b1)
  toChopped 0 = O
  toChopped 1 = I


chop' :: Signal (Unsigned 24) -> Signal' SerialClock Chopped
chop' = (liftA2 go around) . oversampling 0 0 where
  go _ 0 = Done
  go mask n = toChopped (n .&. mask)
  around = register' serialClock (3 :: Unsigned 24) (liftA (`shiftL` 2) around)
  toChopped 0 = O
  toChopped 1 = I

topEntity :: Signal (Unsigned 24) -> Signal' SerialClock Chopped
topEntity = chop


t1 = chop' $ fromList [0, 20, 0, 0]