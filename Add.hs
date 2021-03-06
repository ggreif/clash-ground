{-# LANGUAGE ViewPatterns, LambdaCase, GADTs, PatternSynonyms, KindSignatures, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor #-}

module Add where

-- implement the virtual machine derived in mosaic/HuttonBahr.hs

import CLaSH.Prelude
import qualified Data.List as L
import Data.Maybe
import Control.Arrow (first)
import Debug.Trace (trace, traceShowId)
import Data.Coerce
import Test.QuickCheck
import Data.Map.Strict

data ExpF a = LitF Int | a `AddF` a deriving (Eq, Ord, Functor, Show)
newtype Fix f = Fix (f (Fix f)) -- deriving Show
type Exp = Fix ExpF

instance Eq Exp where
  Fix a == Fix b = a == b

instance Ord Exp where
  Fix a <= Fix b = a <= b

pattern Lit i = Fix (LitF i)
pattern Add a b <- Fix ((coerce -> a) `AddF` (coerce -> b))
  where Add a b = Fix ((coerce a) `AddF` (coerce b))

eval :: CONT k -> Exp -> k
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

-- TODO
 -- http://matt.might.net/papers/vanhorn2010abstract.pdf
 -- make the env/control stack abstract. Ops: get args/deliver result, etc.

-- mealy approach
data Direction = Enter ROM | Return Int deriving Show
type State = (Vec 10 DO, Direction)
data DO = DOHALT | DONEXT ROM | DOADD Int deriving Show
type ROM = Unsigned 10


machine = mealy optadder startState
  where optadder s i = first opt (adder s i)
startState = (repeat DOHALT, Enter 0)
reAst b (a, _) = (a, Enter b)

pattern EnterAdd a b <- Enter (rom' -> a `AddF` b)
pattern EnterLit i <- Enter (rom' -> LitF i)
go a = (trace (show a) a, Nothing)
trivial (Return j) = j
trivial (EnterLit j) = j

adder :: State -> Maybe ROM -> (State, Maybe Int)
adder _ (Just rom) = (reAst rom startState, Nothing) -- reset

adder (done@(DOHALT :> _), Return res) Nothing = ((done, Return res), Just res)
adder (stk, EnterAdd a b) Nothing = go (DONEXT b +>> stk, Enter a)
adder (DOADD i :> stk, (trivial->j)) Nothing = go (stk :< DOHALT, Return $ i+j)
adder (DONEXT rom :> stk, (trivial->i)) Nothing = go (DOADD i :> stk, Enter rom)
--adder (show -> problem) _ = error problem

opt :: State -> State
opt (DONEXT rom :> DOADD i :> stk, Return j) = traceShowId ((DOADD (i+j) :> stk) :< DOHALT, Enter rom)
opt (DOADD i :> DOHALT :> stk, Return j) = traceShowId ((DOHALT :> stk) :< DOHALT, Return (i+j))
opt st = st

class ControlStack cont {-env?-} where
  -- pop, deliver

feed = Just 0 `register` pure Nothing

samp = sampleN 30 $ machine feed

topEntity :: Signal (Maybe ROM) -> Signal (Maybe Int)
topEntity = machine

type AddI = ExpF ROM

rom' :: ROM -> ExpF ROM
rom' 0 = 1 `AddF` 2
rom' 1 = LitF 1
rom' 2 = 3 `AddF` 3
rom' 3 = 4 `AddF` 5
rom' 4 = LitF 40
rom' 5 = LitF 50


---------------
-- TEST HARNESS
---------------

testInput :: Signal (Maybe ROM)
testInput = stimuliGenerator $ Just 0 :> Nothing :> Nothing :> Nil

expectedOutput :: Signal (Maybe Int) -> Signal Bool
expectedOutput = outputVerifier $  Nothing :> Nothing :> Nothing :> Just 71 :> Nil

test = {-Prelude.drop 1-} (sampleN 4 (expectedOutput $ topEntity testInput))


instance Arbitrary Exp where
  arbitrary = frequency [ (3, Lit <$> arbitrary)
                        , (2, Fix <$> (AddF <$> arbitrary <*> arbitrary)) ]

instance Show Exp where
  show (Fix a) = show a


toRom :: [Exp] -> Vec (n+1) (ExpF ROM) -> Vec (n+1) (ExpF ROM)
toRom [] v = v
toRom (Lit i:more) (_:>v) = LitF i:>v
--toRom (a `Add` b:more) (_:>v) = LitF i:>v


nums :: Int -> Map Exp Int -> Exp -> (Map Exp Int, Int)
nums n m l@Lit{} = (insert l n m, n+1)
nums n m p@(a `Add` b) = case Data.Map.Strict.lookup p m of
                           Nothing -> (r, n'')
  where (l, n') = nums (n+1) (insert p n m) a
        (r, n'') = nums (n'+1) l b


{-

Type W = Unsigned 10
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
