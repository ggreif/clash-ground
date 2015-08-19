{-# LANGUAGE ViewPatterns #-}

import CLaSH.Prelude

data Chopped = O | I | Done
  deriving (Show, Read, Bounded, Eq, Ord, Enum)

chop :: Enum a => Signal a -> Signal Chopped
chop = liftA go where
  go (fromEnum -> 0) = Done
  go n = toEnum (fromEnum n .&. 0b1)


topEntity :: Signal (Unsigned 24) -> Signal Chopped
topEntity = chop
