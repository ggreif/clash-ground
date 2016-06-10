{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, ScopedTypeVariables, BinaryLiterals #-}

module I2C'Servant where

import CLaSH.Prelude

class Serve a where
  type Next a
  accept :: Unsigned 8 -> Maybe (Next a)


data Done

data left <|> right

instance (Serve left, Serve right) => Serve (left <|> right) where
  type Next (left <|> right) = Next left `Either` Next right
  accept recvd = undefined -- case (accept :: Unsigned 8 -> Maybe (Next left)) recvd of
                 --  Just l -> Just (Left l)

data Address (addr :: Nat) next

instance (KnownNat addr, Serve next) => Serve (Address addr next) where
  type Next (Address addr next) = next
  accept recvd | fromIntegral recvd == snatToInteger (snat :: SNat addr) = Just undefined
  accept _ = Nothing


data ReadBytes (n :: Nat) next
type ReadOne = ReadBytes 1



data SendBytes (n :: Nat) next
type SendOne = SendBytes 1


-- ** Example: PCA9552

type PCA9552 = Address 0b1100000 (ReadOne Done)

