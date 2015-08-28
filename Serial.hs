{-# LANGUAGE ViewPatterns #-}

import CLaSH.Prelude
import CLaSH.Signal.Explicit
import CLaSH.Prelude.Mealy
import qualified Data.List as List

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
t3 = chop' $ fromList ( twelve 0b101111111111111111111101
                List.++ twelve 0b101100000000000000001101
                List.++ twelve 0b101100111001111001111101
                List.++ twelve 0b000000000000000000000000)

t3' = inverse t3


twelve :: Unsigned 24 -> [Unsigned 24]
twelve n = List.take 12 (cycle [n])

-- using a mealy machine
chop' :: Signal' SerialClock (Unsigned 24) -> Signal' SerialClock Chopped
chop' = mealy' serialClock go start where
  start = (0, 11)
  go (0, 0) i = ((i, 11), Done)
  go (0, offs) _ = ((0, offs - 1), Done)
  go (old, 0) i = ((i, 11), toChopped $ old .&. 0b11)
  go (old, offs) _ = ((old `shiftR` 2, offs - 1), toChopped $ old .&. 0b11)
  toChopped 0 = OO
  toChopped 1 = OI
  toChopped 2 = IO
  toChopped 3 = II


inverse :: Signal' SerialClock Chopped -> Signal' SerialClock (Unsigned 24)
inverse = mealy' serialClock go start where
  start = (0, 0, 11)
  go (acc, _, 0) Done = ((0, acc, 11), rearrange acc)
  go (acc, _, 0) chop = ((0, new, 11), rearrange new) where new = (acc `shiftL` 2) .|. fromChopped chop
  go (acc, old, n) Done = ((acc, old, n - 1), rearrange old)
  go (acc, old, n) chop = (((acc `shiftL` 2) .|. fromChopped chop, old, n - 1), rearrange old)
  fromChopped OO = 0
  fromChopped OI = 1
  fromChopped IO = 2
  fromChopped II = 3


rearrange :: Unsigned 24 -> Unsigned 24
--rearrange = unpack . shuffle . pack where
rearrange = shuffle where
   shuffle a = (setSlice d23 d22 s0 . setSlice d1 d0 s11) a where
     s0 = slice d1 d0 a
     s11 = slice d23 d22 a
