module Recog where

import CLaSH.Prelude
--import Debug.Trace

recog :: (KnownNat (d + 1 + m), KnownNat m) => SNat (d + 1) -> BitVector m -> Signal (BitVector (d + 1 + m)) -> Signal (Maybe (Unsigned 6))
recog d pat s = fold (liftA2 find) (zipWith match poss shifts)
  where find _ b@Just{} = b
        find a _ = a
        shifts = iterate d (`shiftR` 1) s
        poss = iterate d (+1) 0
        match pos shifted = (\sig -> if {-traceShow (sig, pat, pos)-} truncateB sig == pat then Just pos else Nothing) <$> shifted


-- #### TEST BENCH ####

topEntity :: Signal (BitVector 8) -> Signal (Maybe (Unsigned 6))
topEntity = recog d5 0b101 -- check 3 bits


testInput :: Signal (BitVector 8)
testInput = 0 `register` (0b101 `register` (0b10100000 `register` pure 0b1011010))
--infixr `register`

expectedOutput :: Signal (Maybe (Unsigned 6)) -> Signal Bool
expectedOutput = outputVerifier $ Nothing :> Just 0 :> Just 5 :> Just 4 :> Nil

test = takeWhile not . sample $ expectedOutput (topEntity testInput)
