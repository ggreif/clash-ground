import CLaSH.Prelude
import qualified Data.List as L

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        wrEnable = False `register` signal True
        --read = mux (liftA2 (&&) wrEnable same) written $ blockRamPow2 init wrAddr nums wrEnable write
        read = mux same written $ blockRamPow2 init wrAddr nums wrEnable write
        write = read + 1
        written = 9999 `register` write
        wrAddr = 1000 `register` nums
        --same = liftA2 (==) wrAddr (99 `register` nums)
        --same = liftA2 (==) (99 `register` wrAddr) nums
        same = liftA2 (==) wrAddr nums

-- #### TEST BENCH ####

topEntity :: Signal (Unsigned 7) -> Signal (Unsigned 10)
topEntity = histo


testInput, testInput' :: Signal (Unsigned 7)
testInput' = stimuliGenerator $ 1 :> 2 :> 0 :> 2 :> 3 :> 0 :> 3 :> 0 :> 3 :> 4 :> 0 :> 1 :> 0 :> Nil
testInput'' = stimuliGenerator $ 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil
testInput = stimuliGenerator $ 0 :> 0 :> 1 :> 1 :> 2 :> 2 :> 3 :> 3 :> 4 :> 4 :> 5 :> 5 :> 0 :> Nil

expectedOutput :: Signal (Unsigned 10) -> Signal Bool
expectedOutput = outputVerifier $
                --undefined :> 0 :> 0 :> 0 :> 1 :> 0 :> 1 :> 1 :> 2 :> 2 :> 0 :> 3 :> 1 :> 4 :> Nil
                  ---undefined :> 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> Nil
                  undefined :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 2 :> Nil

test = L.takeWhile not . L.drop 1 . sample $ expectedOutput (topEntity testInput)
