{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, ScopedTypeVariables, BinaryLiterals #-}

module I2C'Servant where

import CLaSH.Prelude

class Serve (a :: Bool -> * -> *) where
  type Next a
  accept :: Unsigned 8 -> Maybe (Next a)


data Done

data (left :: Bool -> * -> *) <|> (right :: Bool -> * -> *) :: Bool -> * -> * where

instance (Serve left, Serve right) => Serve (left <|> right) where
  type Next (left <|> right) = Next left `Either` Next right
  accept recvd = undefined -- case (accept :: Unsigned 8 -> Maybe (Next left)) recvd of
                 --  Just l -> Just (Left l)

{-
data Address (addr :: Nat) next

instance (KnownNat addr, Serve next) => Serve (Address addr next) where
  type Next (Address addr next) = next
  accept recvd | fromIntegral recvd == snatToInteger (snat :: SNat addr) = Just undefined
  accept _ = Nothing


data ReadBytes (n :: Nat) next
type ReadOne = ReadBytes 1



data SendBytes (n :: Nat) next
type SendOne = SendBytes 1

-- We whould automatically ack.
--  The type won't contain this junk
-- data AckReceived
-}

-- ** Signatures

debounce :: Signal Bool -> Signal Bool -> Signal (Bool, Bool)
debounce = undefined

flankDetect :: Eq a => Signal a -> Signal (a, Bool)
flankDetect = undefined


bitSlave :: Signal ((Bit, Bool), (Bit, Bool)) -- SDA, SCL + flanks
         -> Signal (Unsigned 8) -- byte to write
         -> Signal Bool -- ACK-out
         -> Signal (Unsigned 8, (Bool, Bool, Bool, Bool), Bit) -- byte read, (START, ACK, NACK, ReSTART), SDA-out
bitSlave = undefined

-- ** Example: PCA9552

--type PCA9552 = Address 0b1100000 (ReadOne Done)

type State = (Unsigned 8, Vec 8 (Unsigned 8))

pca9552 :: Signal ((Bit, Bool), (Bit, Bool)) -- SDA, SCL + flanks
        -> Signal Bit                        -- SDA-out
pca9552 i = o
  where (read, conds, o) = unbundle $ bitSlave i write ack
        (ack, write) = mealyB step (0, CLaSH.Prelude.repeat 0b01010101) (read, conds)
        step :: State -> (Unsigned 8, (Bool, Bool, Bool, Bool)) -> (State, (Bool, Unsigned 8))
        step s i = (s, (True, 0)) -- FIXME
