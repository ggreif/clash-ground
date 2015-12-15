import CLaSH.Prelude
import qualified Data.List as L
import qualified Test.QuickCheck as Q

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        wrEnable = False `register` signal True
        read = blockRamPow2 init wrAddr nums wrEnable (correct <$> nums <*> wrAddr <*> write)
        write = read + 1
        wrAddr = 0 `register` nums
        correct rd wr dat | rd == wr = dat + 1
                          | otherwise = dat
        

-- #### TEST BENCH ####

topEntity :: Signal (Unsigned 7) -> Signal (Unsigned 10)
topEntity = histo


testInput :: Signal (Unsigned 7)
testInput = stimuliGenerator $ 1 :> 2 :> 0 :> 2 :> 3 :> 0 :> 3 :> 0 :> 3 :> 4 :> 0 :> 1 :> 0 :> Nil

expectedOutput :: Signal (Unsigned 10) -> Signal Bool
expectedOutput = outputVerifier $
                  undefined :> 0 :> 0 :> 0 :> 1 :> 0 :> 1 :> 1 :> 2 :> 2 :> 0 :> 3 :> 1 :> 4 :> Nil

test = L.takeWhile not . L.drop 1 . sample $ expectedOutput (topEntity testInput)

prop_histo :: [Unsigned 10] -> Bool
prop_histo [] = True
prop_histo [_] = True
prop_histo as = sample' as (histo $ fromList as) == butLast (L.reverse (snd (histo' as)))
  where

sample' :: [Unsigned 10] -> Signal (Unsigned 12) -> [Unsigned 12]
sample' as = L.tail . sampleN (L.length as)
butLast xs = L.take (L.length xs) xs

histo' = L.foldl inc ([], [])
  where inc (occs, res) num = case L.lookup num occs of
          Just occ -> ((num, occ+1):occs, occ:res)
          Nothing -> ((num, 1):occs, 0:res)
