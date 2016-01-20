module OneToTwo where

import CLaSH.Prelude


encode1x2 :: FiniteBits d => Signal (Vec 2 d) -> Signal (Vec 3 d)
encode1x2 = bundle . (\(a:>b:>Nil)-> a:>b:>(a `xor` b):>Nil) . unbundle


decode1x2 :: FiniteBits d => Signal (Vec 2 (Maybe d)) -> Signal (Maybe (Vec 2 d, Bool))
decode1x2 = undefined
  where select (Just a) (Just b) (Just a'b) = Just (a:>b:>Nil, a `xor` b == a'b)
        select (Just a) (Just b) Nothing = Just (a:>b:>Nil, True)
        select (Just a) Nothing (Just a'b) = Just (a:>a `xor`a'b:>Nil, True)
        select Nothing (Just b) (Just a'b) = Just (b `xor`a'b:>b:>Nil, True)
        select _ _ _ = Nothing
