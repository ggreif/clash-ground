module Histogram where

import CLaSH.Prelude
import qualified Data.List as L
import qualified Test.QuickCheck as Q
import Test.QuickCheck (quickCheck, (===))

{- JUST for reference

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        wrEnable = False `register` signal True
        read = readNew (blockRamPow2 init) wrAddr nums wrEnable write
        write = read + 1
        wrAddr = 0 `register` nums

-}

-- Convenience adapters

maybeWrite :: (Signal addr -> Signal addr -> Signal Bool -> Signal dt -> Signal dt)
           -> Signal (Maybe (addr, dt)) -> Signal addr -> Signal dt
maybeWrite ram wr rd = ram wrAddr rd wrEn wrData
  where apart (Just (addr, dt)) = (True, addr, dt)
        apart Nothing = (False, undefined, undefined)
        (wrEn, wrAddr, wrData) = unbundle (apart <$> wr)

condWrite :: (Signal (Maybe (addr, dt)) -> Signal addr -> Signal dt)
          -> (dt -> Maybe dt) -> Signal addr -> Signal dt
--condWrite ram trans = condSigWrite ram (pure trans)
condWrite ram trans rd = result
  where result = ram wr rd
        rd' = const Nothing `register` fmap fmap ((,) <$> rd)
        wr = rd' <*> (trans <$> result)

condSigWrite :: (Signal (Maybe (addr, dt)) -> Signal addr -> Signal dt)
             -> Signal (dt -> Maybe dt) -> Signal addr -> Signal dt
condSigWrite ram trans rd = result
  where result = ram wr rd
        rd' = const Nothing `register` fmap fmap ((,) <$> rd)
        wr = rd' <*> (trans <*> result)

uncondWrite :: (Signal (Maybe (addr, dt)) -> Signal addr -> Signal dt)
          -> (dt -> dt) -> Signal addr -> Signal dt
uncondWrite ram trans = condWrite ram (Just . trans)

uncondSigWrite :: (Signal (Maybe (addr, dt)) -> Signal addr -> Signal dt)
          -> Signal (dt -> dt) -> Signal addr -> Signal dt
uncondSigWrite ram trans = condSigWrite ram ((Just .) <$> trans)

-- blockRam-backed Moore machine?
--
condUpdater :: (Signal (dt -> Maybe dt) -> Signal addr -> Signal dt)
            -> (i -> addr) -> (dt -> o) -> Signal (o -> Maybe (dt -> dt)) -> Signal i -> Signal o
condUpdater ram hash out upd inp = result
  where result = out <$> dt
        rd = hash <$> inp
        dt = ram wr rd
        wr = fmap match upd <*> result
        match f o dt = ($ dt) <$> f o        

class FunctionLike f where
  type Func f
  refunc :: f -> Func f
  refuncSig :: Signal f -> Signal (Func f)
  refuncSig = fmap refunc

-- Experimental manually defunctionalized version
condUpdater' :: ( FunctionLike out, Func out ~ (dt -> o)
                , FunctionLike upd, Func upd ~ (o -> Maybe (dt -> dt)))
             => (Signal (dt -> Maybe dt) -> Signal addr -> Signal dt)
            -> Signal out -> Signal upd -> Signal addr -> Signal o
condUpdater' ram out upd rd = result
  where result = refuncSig out <*> dt
        dt = ram wr rd
        wr = fmap match (refuncSig upd) <*> result
        match f o dt = ($ dt) <$> f o        

data Uncond (b :: Nat) = PlusOne

instance KnownNat b => FunctionLike (Uncond b) where
  type Func (Uncond b) = Unsigned b -> Unsigned b
  refunc PlusOne = (+1)

-- rewrite histo in terms of condWrite

histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo = uncondWrite (maybeWrite $ readNew (blockRamPow2 (repeat 0))) (+1)

--temporarily:

--readNew = id

-- #### TEST BENCH ####

topEntity :: Signal (Unsigned 7) -> Signal (Unsigned 10)
topEntity = histo


testInput, testInput', testInput'' :: Signal (Unsigned 7)
testInput'' = stimuliGenerator $ 1 :> 2 :> 0 :> 2 :> 3 :> 0 :> 3 :> 0 :> 3 :> 4 :> 0 :> 1 :> 0 :> Nil
testInput' = pure 0
testInput = stimuliGenerator $ 0 :> 0 :> 1 :> 1 :> 2 :> 2 :> 3 :> 3 :> 4 :> 4 :> 5 :> 5 :> 0 :> Nil

expectedOutput :: Signal (Unsigned 10) -> Signal Bool
expectedOutput = outputVerifier $
                  ---undefined :> 0 :> 0 :> 0 :> 1 :> 0 :> 1 :> 1 :> 2 :> 2 :> 0 :> 3 :> 1 :> 4 :> {- 5 :> -} Nil
                  ---undefined :> 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> Nil
                  undefined :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 0 :> 1 :> 2 :> Nil

test = L.takeWhile not . L.drop 1 . sample $ expectedOutput (topEntity testInput)

qc = quickCheck prop_histo

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
