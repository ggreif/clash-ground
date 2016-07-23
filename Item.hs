{-# language GADTSyntax #-}
module Item where

import CLaSH.Prelude
import qualified Data.List as L


data Wagon where
  Applicator :: Int -> Wagon
  Abstractor :: (BitVector 10) -> Wagon
  Hole :: Wagon
  Stop :: Wagon
  
type Train = (Vec 20 Wagon, Vec 20 (Vec 10 Int))

eval :: Train -> () -> (Train, Bool)
eval (ws, is) _ = undefined

meval = mealy eval start
  where start = (repeat $ Applicator 1, repeat $ repeat 0)
