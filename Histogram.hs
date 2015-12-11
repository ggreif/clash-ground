import CLaSH.Prelude



histo :: (KnownNat n, KnownNat (2 ^ n), KnownNat b) => Signal (Unsigned n) -> Signal (Unsigned b)
histo nums = read
  where init = repeat 0
        delayedWr = False `register` signal True
        write = 0 `register` (read + 1)
        read = blockRamPow2 init nums wrAddr delayedWr write
        wrAddr = 0 `register` nums



