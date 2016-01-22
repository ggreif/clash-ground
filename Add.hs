{-# LANGUAGE ViewPatterns, LambdaCase, RankNTypes, GADTs, PatternSynonyms, KindSignatures, MultiParamTypeClasses, FlexibleInstances #-}

module Add where

-- implement the virtual machine derived in mosaic/HuttonBahr.hs

import CLaSH.Prelude
import qualified Data.List as L
import Data.Maybe

data Exp = Lit Int | Exp `Add` Exp deriving Show

type Eval exp = forall k . CONT k -> exp -> k

eval :: Eval Exp
eval c (\case Lit (exec c -> n') -> n'
              (eval (NEXT c) -> a') `Add` (a' -> b') -> b'
        -> res') = res'


data CONT k where
  ADD :: CONT k -> Int -> CONT k
  NEXT :: CONT k -> CONT (Exp -> k)
  HALT :: CONT Int

exec :: CONT k -> Int -> k
exec (ADD c a) (exec c . (a+) -> res) = res
exec (NEXT (ADD -> c)) (eval . c -> res) = res
exec HALT a = a


-- mealy approach

type State = (Vec 10 DO, ROM, Maybe Int)
data DO = DOHALT | DONEXT ROM | DOADD Int deriving Show
type ROM = Unsigned 10


machine = mealy adder startState
startState = (repeat DOHALT, 0, Nothing)
reast b (a, _, c) = (a, b, c)

adder :: State -> Maybe ROM -> (State, Maybe Int)
adder _ (Just rom) = (reast rom startState, Nothing)
--adder _ (Just (ram' -> ast)) = (reast ast startState, Nothing)
adder (done@(DOHALT:>_), _, res@Just{}) Nothing = ((done, 0, res), res)
adder (DONEXT rom:> stk, (ram'->Left i), Nothing) Nothing = ((DOADD i:>stk, rom, Nothing), Nothing)
adder (stk, (ram'->Right (a,b)), Nothing) Nothing = ((DONEXT b+>>stk, a, Nothing), Nothing)
--adder (DONEXT _:> stk, (ram'->Right (a,b)), Just i) Nothing = ((DOADD i:>stk, 0, Nothing), Nothing)
adder (DOADD i :> stk, (ram'->Left j), Nothing) Nothing = ((stk:<DOHALT, 0, Just $ i+j), Nothing)

feed = Just 2 `register` pure Nothing

samp = sampleN 30 $ machine feed

-- RAM-backed expression tree
type EXP addr = addr -> Either Int (addr, addr)

topEntity :: Signal (Maybe (Unsigned 10)) -> Signal (Maybe Int)
topEntity _ = snd <$> eval (pure (0, 0)) (pure 6666)
  where eval :: (Eq sp, Num sp) => Signal (addr, sp) -> Signal Int -> Signal (Int, Maybe Int)
        eval = liftA2 (uncurry eval')
        eval' :: (Eq sp, Num sp) => {-Either Int (addr, addr)-}addr -> sp -> Int -> (Int, Maybe Int)
        eval' exp 0 acc = (acc, Just acc)
        ram :: (Eq addr, Num addr) => Signal addr -> Signal (Either Int (addr, addr))
        ram addr = undefined `register` liftA ram' addr -- simulate block ram



ram' 0 = Right (1, 2)
ram' 1 = Left 1
ram' 2 = Right (3, 4)
ram' 3 = Left 30
ram' 4 = Left 40


---------------
-- TEST HARNESS
---------------

testInput = stimuliGenerator $ Just 0 :> Nothing :> Nothing :> Nil

expectedOutput = outputVerifier $  Nothing :> Nothing :> Nothing :> Just 71 :> Nil

test = {-Prelude.drop 1-} (sampleN 4 (expectedOutput $ topEntity testInput))


{-

type W = Unsigned 10
type DBBits = 4
type DB = 2^DBBits
type DBI = Unsigned DB

data Instr = LOAD W  -- absolute
           | STORE W -- upmost row
           | LEFT DBI -- follow
           | RIGHT DBI
           | BRANCH DBI
           | NOP
           | STOP
           | RE
           | AD DBI


type DeBruijn = Vec DB

dbi :: Vec (DB-1) a -> DBI -> a
dbi = (!!)

redox :: DeBruijn (W, W, W) -> Instr -> (DeBruijn (W, W, W), Maybe W)
redox ((_, b, c):>(m:<l)) (LOAD addr) = (l:>(addr, b, c):>m, Nothing)
  --where blockRamPow2 (repeat 0)


redox (h:>(m:<l)) NOP = (l:>h:>m, Nothing)
redox ((a,b,c):>m) (AD (dbi m -> (d,e,f))) = ((a+d,b+e,c+f):>m, Nothing)
  --where (d,e,f) = m !! dbi
redox (h:>m) RE = (m:<h, Nothing)
redox v STOP = (v, theLook v)
  where theLook ((a, _, _):>_) = Just a

topEntity = mealy redox (repeat (0,0,0))

testInput = stimuliGenerator $ LOAD 11 :> LOAD 3 :> RE :> AD 0 :> STOP :> Nil

t = sampleN 30 $ topEntity testInput


-- ## WARM UP
data Na = Z | S Na deriving Show

pls Z = id
pls (S n) = pls n . S

-- We need something like this

pls', s', id' :: Machine a => a
pls' = convention (\arg -> untag arg ((\() -> id') :! (\n -> call pls' n {- . s' -}) :! Empy))
s' = convention(\arg -> same arg (tag 1 arg))
 where same :: a -> a -> a
       same _ = id
diag' = convention(\arg -> p arg (tag 0 (arg,arg)))
 where p :: a -> a -> a
       p _ = id
id' = convention(\arg -> arg)

class Machine a where
  -- calling convention
  convention :: Args t a => (t -> a) -> a
  tag :: Args t a => Int -> t -> a
  untag :: a -> Cases n a -> a
  call :: a -> a -> a

class Machine a => Args t a where
  nth :: t -> Int -> Maybe a
  pck :: [a] -> t

instance {-# INCOHERENT #-} Machine a => Args a a where
  nth a 0 = Just a
  nth _ _ = Nothing
  pck (a:_) = a

instance Machine a => Args () a where
  nth _ _ = Nothing
  pck _ = ()

instance Machine a => Args (a, a) a where
  nth (b, _) 0 = Just b
  nth (_, c) 1 = Just c
  nth _ _ = Nothing
  pck (b:c:_) = (b, c)


data Cases :: Na -> * -> * where
  Empy :: Cases Z a
  Case :: Args t a => (t -> a) -> Cases n a -> Cases (S n) a

instance Show (Cases n Binding) where
  show Empy = ""
  show (Case f more) = show (BConv f) L.++ " :! " L.++ show more

pattern f :! cs = Case f cs
infixr 4 :!
infixr 4 `Case`


data Binding where
  B :: Binding
  BConv :: Args t Binding => (t -> Binding) -> Binding
  BTag :: Args t Binding => Int -> t -> Binding
  BCall :: Binding -> Binding -> Binding
  BUntag :: Binding -> Cases n Binding -> Binding

instance Show Binding where
  show B = "B"
  show (BConv f) = "CONV(B)->" L.++ show (f $ pck $ L.repeat B)
  show (BTag i t) = "TAG(" L.++ show i L.++ ":" L.++ L.intersperse ' ' (L.concat $ L.map ((show :: Binding -> String) . fromJust) $ takeWhile (not . null) $ L.map (t `nth`) [0 ..]) L.++ ")"
  show (BCall f a) = "call " L.++ show f L.++ "(" L.++ show a L.++ ")"
  show (BUntag b cs) = "untag " L.++ show b L.++ " as (" L.++ show cs L.++ ")"

instance Machine Binding where
  convention = BConv
  
  tag = BTag
  call = BCall
  untag = BUntag

-- Serialize such a beast to a Vec

-- remember: lazy mantra is... LET is allocation, CASE is evaluation
-- TODO: we need a system to break cycles

--newtype Heap = HP (Unsigned 10 -> Vec (2^10) (W, W, W) -> (Vec (2^10) (W, W, W), Unsigned 10))
newtype Heap = HP ([(W, W, W)]) deriving Show

instance Machine Heap where
  -- function entry point
  convention f = HP $ (1, 3 {-this should be (compile f) as code-}, 0) : compile f
    where compile f = []
  
-}
