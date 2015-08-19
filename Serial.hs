{-# LANGUAGE ViewPatterns #-}

import CLaSH.Prelude

data Chopped = O | I | Done
  deriving (Show, Read, Bounded, Eq, Ord, Enum)

--chop :: Enum a => Signal a -> Signal Chopped
chop :: Signal (Unsigned 24) -> Signal Chopped
chop = liftA go where
  go 0 = Done
  go n = toChopped (n .&. 0b1)
  toChopped 0 = O
  toChopped 1 = I

topEntity :: Signal (Unsigned 24) -> Signal Chopped
topEntity = chop
