import CLaSH.Prelude
import qualified Data.List as L

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        delayedWr = toSignal $ delay (False :> False :> Nil) $ signal True
        --delayedWr' = False `register` signal True
        write = toSignal $ (0 :> Nil) `delay` fromSignal (read + 1)
        read = blockRamPow2 init nums wrAddr delayedWr write
        --read = asyncRamPow2 nums wrAddr delayedWr write
        wrAddr = toSignal $ (0 :> 0 :> Nil) `delay` fromSignal nums

-- #### TEST BENCH ####

topEntity :: Signal (Unsigned 7) -> Signal (Unsigned 10)
topEntity = histo


testInput :: Signal (Unsigned 7)
testInput = stimuliGenerator $ 1 :> 2 :> 2 :> 3 :> 3 :> 3 :> 4 :> 0 :> 1 :> Nil

expectedOutput :: Signal (Unsigned 10) -> Signal Bool
expectedOutput = outputVerifier $ 0 :> 0 :> 1 :> 0 :> 1 :> 2 :> 0 :> 0 :> 1 :> Nil

--test = L.drop 2 $ sampleN 30 $ expectedOutput (topEntity testInput)
test = L.drop 2 $ sampleN 30 $ (topEntity testInput)
