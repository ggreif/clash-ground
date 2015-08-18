module ALUTypes where

import CLaSH.Prelude

data OP = Add | Sub
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

deriveLift ''OP
--instance Lift OP where
--  lift a = undefined