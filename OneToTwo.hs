module OneToTwo where

import CLaSH.Prelude


encode1x2 :: FiniteBits d => Signal (Vec 2 d) -> Signal (Vec 3 d)
encode1x2 = bundle . (\(a:>b:>Nil)-> a:>b:>(a `xor` b):>Nil) . unbundle


decode1x2 :: FiniteBits d => Signal (Vec 3 (Maybe d)) -> Signal (Maybe (Vec 2 d, Bool))
decode1x2 = selectV3 . unbundle
  where select (Just a) (Just b) extra = Just (a:>b:>Nil, case extra of Just a'b -> a `xor` b == a'b; Nothing -> True)
        select (Just a) Nothing (Just a'b) = Just (a :> a `xor` a'b :> Nil, True)
        select Nothing (Just b) (Just a'b) = Just (a'b `xor` b :> b:> Nil, True)
        select _ _ _ = Nothing
        selectV3 :: FiniteBits d => Vec 3 (Signal (Maybe d)) -> Signal (Maybe (Vec 2 d, Bool))
        selectV3 (a:>b:>a'b:>Nil) = select <$> a <*> b <*> a'b

i0,i1 :: Signal (Unsigned 5)
i0 = 7 `register` i1 --pure 7
i1 = 0xe `register` (i0 + i1) -- pure 0xe
inp = encode1x2 (bundle (i0:>i1:>Nil))

inp' = fmap Just <$> inp -- happy unperturbed channels
outp = decode1x2 inp'

inp'' = fmap (fmap (+1)) <$> inp' -- introduces bit-errors on all three channels
outp'' = decode1x2 inp''


inp''' = (\(a:>_:>c:>Nil) -> a:>Nothing:>c:>Nil) <$> inp' -- loss of channel b
outp''' = decode1x2 inp'''

topEntity :: Signal (Vec 2 (Unsigned 12)) -> Signal (Maybe (Vec 2 (Unsigned 12), Bool))
topEntity = decode1x2 . fmap (fmap Just) . encode1x2
