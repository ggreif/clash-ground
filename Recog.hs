{-# LANGUAGE PartialTypeSignatures #-}

module Recog where

import CLaSH.Prelude

recog :: KnownNat m => SNat (d + 1) -> BitVector m -> Signal (BitVector (d + 1 + m)) -> Signal (Maybe (Unsigned 6))
recog d pat s = fold find (zipWith match poss comps)
  where find a _ = a
        --comps :: Vec (d + 1) (Signal )
        comps = iterate d id s
        --poss = indices d --iterate d (+1) 0
        poss :: Vec _ (Unsigned 6)
        poss = iterate d (+1) 0
        match :: Unsigned 6 -> Signal (BitVector _) -> Signal (Maybe (Unsigned 6))
        match pos sig = (\sig -> if truncateB sig == pat then Just pos else Nothing) <$> sig


-- #### TEST BENCH ####

topEntity :: Signal (BitVector 8) -> Signal (Maybe (Unsigned 6))
topEntity = recog d3 0b101


testInput :: Signal (BitVector 8)
testInput = pure 0b101

expectedOutput :: Signal (Maybe (Unsigned 6)) -> Signal Bool
expectedOutput = outputVerifier $ Just 0 :> Just 0 :> Nil

test = takeWhile not . sample $ expectedOutput (topEntity testInput)
