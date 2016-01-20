module OneToTwo where

import CLaSH.Prelude


encode1x2 :: FiniteBits d => Signal (Vec 2 d) -> Signal (Vec 3 d)
encode1x2 = bundle . (\(a:>b:>Nil)-> a:>b:>(a `xor` b):>Nil) . unbundle
