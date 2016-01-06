module Histogram where

import CLaSH.Prelude
import qualified Data.List as L
import qualified Test.QuickCheck as Q
import Test.QuickCheck (quickCheck, (===))

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        wrEnable = False `register` signal True
        read = readNew (blockRamPow2 init) wrAddr nums wrEnable write
        write = read + 1
        wrAddr = 0 `register` nums

-- Convenience

maybeWrite :: (Signal addr -> Signal addr -> Signal Bool -> Signal dt -> Signal dt)
           -> Signal (Maybe (addr, dt)) -> Signal addr -> Signal dt
maybeWrite ram wr rd = ram wrAddr rd wrEn wrData
  where apart (Just (addr, dt)) = (True, addr, dt)
        apart Nothing = (False, undefined, undefined)
        (wrEn, wrAddr, wrData) = unbundle (apart <$> wr)

condWrite :: (Signal (Maybe (addr, dt)) -> Signal addr -> Signal dt)
          -> (dt -> Maybe dt) -> Signal addr -> Signal dt
condWrite ram trans rd = result
  where result = ram wr rd
        rd' = const Nothing `register` fmap fmap ((,) <$> rd)
        wr = rd' <*> (trans <$> result)

-- rewrite histo in terms of condWrite

histoCond :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histoCond = condWrite (maybeWrite $ readNew (blockRamPow2 (repeat 0))) (Just . (+1))

--temporarily:

readNew = id

-- #### TEST BENCH ####

topEntity :: Signal (Unsigned 7) -> Signal (Unsigned 10)
topEntity = histoCond


testInput, testInput', testInput'' :: Signal (Unsigned 7)
testInput = stimuliGenerator $ 1 :> 2 :> 0 :> 2 :> 3 :> 0 :> 3 :> 0 :> 3 :> 4 :> 0 :> 1 :> 0 :> Nil
testInput'' = pure 0
testInput' = stimuliGenerator $ 0 :> 0 :> 1 :> 1 :> 2 :> 2 :> 3 :> 3 :> 4 :> 4 :> 5 :> 5 :> 0 :> Nil

expectedOutput :: Signal (Unsigned 10) -> Signal Bool
expectedOutput = outputVerifier $
                  undefined :> 0 :> 0 :> 0 :> 1 :> 0 :> 1 :> 1 :> 2 :> 2 :> 0 :> 3 :> 1 :> 4 :> {- 5 :> -} Nil
                  ---undefined :> 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> Nil
                  ---undefined :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 2 :> Nil

test = L.takeWhile not . L.drop 1 . sample $ expectedOutput (topEntity testInput)

prop_histo :: [Unsigned 10] -> Q.Property
prop_histo [] = True === True
prop_histo [_] = True === True
prop_histo as = sample' (histo $ fromList as) === butLast (histo' as)
  where sample' :: Signal (Unsigned 12) -> [Unsigned 12]
        sample' = L.tail . sampleN (L.length as)
        butLast xs = L.take (L.length xs - 1) xs

histo' :: [Unsigned 10] -> [Unsigned 12]
histo' = L.drop 1 . fmap (snd . L.head) . L.scanl inc []
  where inc occs num = case L.lookup num occs of
          Just occ -> (num, occ+1):occs
          Nothing -> (num, 0):occs
