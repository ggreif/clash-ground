{-# LANGUAGE ViewPatterns #-}


import CLaSH.Prelude
import ALUTypes


alu :: OP -> Signed 9 -> Signed 9 -> Signed 9
alu Add a1 a2 = a1 + a2
alu Sub a1 a2 = a1 - a2


topEntity :: Signal OP -> Signal (Signed 9) -> Signal (Signed 9) -> Signal (Signed 9)
topEntity = liftA3 alu

fetch :: Signal Word -> Signal Instr
fetch = liftA go where
   go 0 = Alu Add 4 5
   go 1 = Alu Sub 42 25
   go 2 = Stop


data Registers = R { acc :: Signed 9, l :: Signed 9, r :: Signed 9 }
  deriving Show

emptyRegs = R 0 0 0

processor :: Registers -> Signal Instr -> Signal Registers
processor = mealy step where
  step rs (Alu op a1 a2) = (rs{acc = alu op a1 a2}, rs)
  step rs Stop = (rs, rs)


testInput :: Signal (OP,Signed 9,Signed 9)
testInput = stimuliGenerator $(v [(Add,1 :: Signed 9,1 :: Signed 9),(Sub,2,2),(Add,3,3),(Sub,4,4)])

ub :: Signal (a, b, c) -> (Signal a -> Signal b -> Signal c -> Signal d) -> Signal d
ub (unbundle -> (a, b, c)) f = f a b c

expectedOutput :: Signal (Signed 9) -> Signal Bool
expectedOutput = outputVerifier $(v [2 :: Signed 9,0,6,0])
