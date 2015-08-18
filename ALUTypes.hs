module ALUTypes where

import CLaSH.Prelude

data OP = Add | Sub
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

deriveLift ''OP


data Instr = Alu OP (Signed 9) (Signed 9)
           | Stop
  deriving (Show, Eq, Ord)


deriveLift ''Instr
