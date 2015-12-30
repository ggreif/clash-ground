module Recog where

import CLaSH.Prelude
--import Debug.Trace

recog :: (KnownNat (d + 1 + m), KnownNat m) => SNat (d + 1) -> BitVector m -> Signal (BitVector (d + 1 + m)) -> Signal (Maybe (Unsigned 6))
recog d pat s = fold (liftA2 find) (zipWith match poss shifts)
  where find a@Just{} _ = a
        find _ b = b
        shifts = iterate d (`shiftR` 1) s
        poss = iterate d (+1) 0
        match pos shifted = (\sig -> if truncateB sig == pat then Just pos else Nothing) <$> shifted


-- #### TEST BENCH ####

topEntity :: Signal (BitVector 8) -> Signal (Maybe (Unsigned 6))
topEntity = recog d3 0b101


testInput :: Signal (BitVector 8)
testInput = pure 0b1010

expectedOutput :: Signal (Maybe (Unsigned 6)) -> Signal Bool
expectedOutput = outputVerifier $ Just 0 :> Just 0 :> Nil

test = takeWhile not . sample $ expectedOutput (topEntity testInput)
