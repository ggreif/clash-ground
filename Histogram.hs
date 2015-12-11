import CLaSH.Prelude
import qualified Data.List as L

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        delayedWr = toSignal $ delay (False :> Nil) $ signal True
        write = toSignal $ (Nil) `delay` fromSignal (read + 1)
        read = blockRamPow2 init wrAddr nums delayedWr write
        wrAddr = toSignal $ (0 :> Nil) `delay` fromSignal nums

-- #### TEST BENCH ####

topEntity :: Signal (Unsigned 7) -> Signal (Unsigned 10)
topEntity = histo


testInput :: Signal (Unsigned 7)
testInput = stimuliGenerator $ 1 :> 2 :> 0 :> 2 :> 3 :> 0 :> 3 :> 0 :> 3 :> 4 :> 0 :> 1 :> 0 :> Nil

expectedOutput :: Signal (Unsigned 10) -> Signal Bool
expectedOutput = outputVerifier $ undefined :>
                               0 :> 0 :> 0 :> 1 :> 0 :> 1 :> 1 :> 2 :> 2 :> 0 :> 3 :> 1 :> 4 :> Nil

test = L.drop 1 $ sampleN 15 $ expectedOutput (topEntity testInput)
--test = L.drop 1 $ sampleN 30 $ (topEntity testInput)
