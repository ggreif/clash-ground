import CLaSH.Prelude
import qualified Data.List as L

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        delayedWr = False `register` signal True
        write = 0 `register` (read + 1)
        read = blockRamPow2 init nums wrAddr delayedWr write
        wrAddr = 0 `register` nums

-- #### TEST BENCH ####

topEntity :: Signal (Unsigned 7) -> Signal (Unsigned 10)
topEntity = histo


testInput :: Signal (Unsigned 7)
testInput = stimuliGenerator $ 1 :> 2 :> 2 :> 3 :> 3 :> 3 :> 4 :> 0 :> 1 :> Nil

expectedOutput :: Signal (Unsigned 10) -> Signal Bool
expectedOutput = outputVerifier $ 0 :> 0 :> 1 :> 0 :> 1 :> 2 :> 0 :> 0 :> 1 :> Nil

test = L.drop 1 $ sampleN 30 $ expectedOutput (topEntity testInput)
