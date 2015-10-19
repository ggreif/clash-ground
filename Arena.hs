module Arena where

import qualified Data.List as L
import CLaSH.Prelude
import CLaSH.Prelude.BlockRam

type BitWidth = 4

load :: (KnownNat n, KnownNat tags) => SNat n -> SNat tags -> Signal (Unsigned BitWidth) -> Signal (Unsigned 18)
load _ _ addr = blockRamPow2 init addr addr read (signal 0) where
    init = 0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> 8 :> 9 :> 10 :> 11 :> 12 :> 13 :> 14 :> 15 :> Nil
    read = signal False
