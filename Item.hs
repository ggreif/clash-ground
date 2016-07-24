{-# language GADTSyntax, LambdaCase #-}

module Item where

import CLaSH.Prelude
import qualified Data.List as L
import Data.Bool
import Debug.Trace


data Wagon where
  Applicator :: Int -> Wagon
  Abstractor :: (BitVector 10) -> Wagon
  Hole :: Wagon
  Stop :: Wagon
  Junk :: Wagon
 deriving Show

type Train = (Vec 20 Wagon, Vec 20 (Vec 10 (Maybe Int)))

eval :: Train -> () -> (Train, Bool)
eval (ws, is) _ = traceShowId ((ws', is'), done ws')
  where pairing = zipWith3 exam is ws (left ws)
        left (ws :> tail) = tail :< ws
        done (Stop :> _) = True
        done _ = False
        ws' = left (combine (fst <$> pairing))
        is' = snd <$> pairing
        combine wws = inserter <$> ws <*> rws
        
        exam _ (Applicator i) (Abstractor bs) = ((Junk, Junk), bool Nothing (Just i) <$> (unpack bs :: Vec 10 Bool))
        exam is w n  = ((w, n), is)

meval = mealy eval start
  where start = (repeat (Applicator 1) :< Abstractor 0b11 :< Hole :< Hole :< Stop, repeat $ repeat Nothing)
