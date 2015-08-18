{-# LANGUAGE ViewPatterns #-}


import CLaSH.Prelude
import ALUTypes


alu :: OP -> Signed 9 -> Signed 9 -> Signed 9
alu Add a1 a2 = a1 + a2
alu Sub a1 a2 = a1 - a2


topEntity :: Signal OP -> Signal (Signed 9) -> Signal (Signed 9) -> Signal (Signed 9)
topEntity = liftA3 alu



data Instr = Alu OP (Signed 9) (Signed 9)
           | Stop
  deriving Show


fetch :: Signal Word -> Signal Instr
fetch = liftA go where
   go 0 = Alu Add 4 5
   go 1 = Stop


testInput :: Signal (OP,Signed 9,Signed 9)
testInput = stimuliGenerator $(v [(Add,1 :: Signed 9,1 :: Signed 9),(Sub,2,2),(Add,3,3),(Sub,4,4)])

ub :: Signal (a, b, c) -> (Signal a -> Signal b -> Signal c -> Signal d) -> Signal d
ub (unbundle -> (a, b, c)) f = f a b c

expectedOutput :: Signal (Signed 9) -> Signal Bool
expectedOutput = outputVerifier $(v [2 :: Signed 9,0,6,0])
