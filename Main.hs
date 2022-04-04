{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module RecursionSchemes where

import Prelude hiding (pi, repeat, head, tail, filter, iterate, succ)

import Control.Monad.ST
import Data.STRef
import Control.Monad
import Control.Applicative (liftA2)
import Data.Nat
import Data.Array.IArray

{- (Co)-Fixpoints and (Co)-Free Monads -}
data Fix f = In { inOp :: f(Fix f) }
data CoFix f = OutOp { out :: f (CoFix f) }

data CoFree g a = CoFree a (g (CoFree g a)) deriving Functor
data Free f a = Var a | Op (f (Free f a)) deriving Functor

coeval :: Functor g => (x -> g x) -> x -> CoFree g x
coeval coalg x = CoFree x (fmap (coeval coalg) (coalg x))

epsilon :: CoFree g a -> a
epsilon (CoFree x _) = x

ctail :: CoFree g a -> g (CoFree g a)
ctail (CoFree _ y) = y

comult :: Functor g => CoFree g a -> CoFree g (CoFree g a)
comult = coeval ctail

{- Natural Numbers -}
data NatF k = ZeroF | SuccF k deriving Functor

instance (Num (Fix NatF)) where
  (+) :: Fix NatF -> Fix NatF -> Fix NatF
  x + y = undefined

instance (Show (Fix NatF)) where
  show :: Fix NatF -> String
  show n = show (toInt n)
    where
      toInt :: Fix NatF -> Int
      toInt (In ZeroF) = 0
      toInt (In (SuccF n)) = toInt n + 1

succ :: Fix NatF -> Fix NatF
succ n = In (SuccF n)

zero :: Fix NatF
zero = In ZeroF
one = succ zero
two = succ one

{- Fold -}
fold :: Functor f => (f b -> b) -> Fix f -> b
fold alg = alg . fmap (fold alg) . inOp

(/\) :: (c -> a) -> (c -> b) -> c -> (a, b)
(f /\ g) x = (f x, g x)

fmapPLeft :: (x -> x') -> (x, y) -> (x', y)
fmapPLeft f (x, y) = (f x, y)

{- Paramorphism -}
para :: Functor f => (f (b, Fix f) -> b) -> Fix f -> b
para alg = alg . (fmap ((para alg) /\ id)) . inOp

{- Fold with parameters -}
{- (- x A) ⊣ (-)^{A} -}
foldParam :: Functor f => ((f (a -> b), a) -> b) -> (Fix f, a) -> b
foldParam alg = alg . fmapPLeft (fmap ((\g -> (foldParam alg) . g) . (\x y -> (x,y))) . inOp)

proj1 :: (a, b) -> a
proj1 (x, y) = x

{- Left Mutumorphism -}
mutu :: Functor f => (f (a, b) -> a) -> (f (a, b) -> b) -> Fix f -> (a, b)
mutu algLeft algRight = (algLeft /\ algRight) . fmap (mutu algLeft algRight) . inOp

{- Recursion Schemes from CoMonads -}
rsfc :: forall g b. Functor g =>
                (forall x. NatF (CoFree g x) -> CoFree g (NatF x)) ->
                (NatF (CoFree g b) -> b) ->
                Fix NatF -> b
rsfc lambda calg = epsilon . fold (fmap calg . lambda . (fmap comult))

ackermann :: Nat -> Nat -> Nat
ackermann 0     m       = S m
ackermann (S n) 0       = ackermann n 1
ackermann (S n) (S m)   = ackermann n (ackermann (S n) m)

ackFolded :: Fix NatF -> Fix NatF -> Fix NatF
ackFolded = fold alg
  where
    alg :: NatF (Fix NatF -> Fix NatF) -> Fix NatF -> Fix NatF
    alg ZeroF m         = succ m
    alg (SuccF g) m     = fold (alg' g) m
    alg' :: (Fix NatF -> Fix NatF) -> NatF (Fix NatF) -> Fix NatF
    alg' g ZeroF      = g one
    alg' g (SuccF m') = g m'

{- Original -}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{- Adjoint folds: + ⊣ △ -}
mutuFib :: Int -> Int
mutuFib 0 = 0
mutuFib n = mutuFib' (n - 1)

mutuFib' :: Int -> Int
mutuFib' 0 = 1
mutuFib' n = mutuFib' (n - 1) + mutuFib (n - 1)

mutuFibRC :: Fix NatF -> Fix NatF
mutuFibRC = proj1 . (mutu algLeft algRight)
  where
    algLeft :: NatF (Fix NatF, Fix NatF) -> Fix NatF
    algLeft ZeroF = zero
    algLeft (SuccF (n, m)) = m
    algRight :: NatF (Fix NatF, Fix NatF) -> Fix NatF
    algRight ZeroF = one
    algRight (SuccF (n, m)) = m + n

tabulate :: (Ix i) => (i,i) -> (i -> a) -> Array i a
tabulate ixs f = array ixs [(i, f i) | i <- range ixs]

{- Fibonacci Dynamic Programming -}
fibDyn :: Int -> Int
fibDyn n = table!n
  where
    table    = tabulate (0, n) fibAux
    fibAux 0 = 0
    fibAux 1 = 1
    fibAux n = table!(n - 1) + table!(n - 2)

lambda :: NatF (CoFree NatF a) -> CoFree NatF (NatF a)
lambda ZeroF                 = CoFree ZeroF ZeroF
lambda (SuccF (CoFree x c))  = CoFree (SuccF x) ((fmap (fmap SuccF)) c)

{- Iteratively constructing the sequence -}
fib' :: Int -> Int 
fib' n = loop n 0 1
     where
       loop 0 x _ = x
       loop m x y = loop (m - 1) y (y + x)

{- Stateful Fibonacci -}
fibST :: Int -> forall s. ST s Int 
fibST n =
  do {
    rx <- newSTRef 0;
    ry <- newSTRef 1;
    let loop 0 = readSTRef rx
        loop n = do {
             x <- readSTRef rx;
             y <- readSTRef ry;
             writeSTRef rx y;
             writeSTRef ry (x + y);
             loop (n - 1)
        }
    in (loop n)
  }

{- Streams of Fibonacci numbers -}
data Stream a = a :< Stream a

instance Functor Stream where
  fmap :: (a -> b) -> Stream a -> Stream b
  fmap f (x :< xs) = f x :< (fmap f xs)

instance Applicative Stream where
  pure x = repeat x
  (<*>) :: Stream (a -> b) -> Stream a -> Stream b
  (f :< fs) <*> (x :< xs) = f x :< fs <*> xs

instance Num a => Num (Stream a) where
  (+) :: Stream a -> Stream a -> Stream a
  (x :< xs) + (y :< ys) = (x + y) :< (xs + ys)
  (*) = liftA2 (*)
  (-) = liftA2 (-)

  abs = fmap abs
  signum = fmap signum
  fromInteger = repeat . fromInteger

repeat :: a -> Stream a
repeat x = x :< repeat x

list :: Int -> Stream a -> [a]
list 0 _ = []
list n (x :< xs) = x : list (n - 1) xs

infix 3 :<

instance Show a => Show (Stream a) where
  show xs = (show (list 10 xs))

class Functor f => Multplicative f where
  unit :: f ()
  (><) :: f a -> f b -> f (a, b)

instance Multplicative Stream where
  unit = repeat ()
  (x :< xs) >< (y :< ys) = (x, y) :< (xs >< ys)

fibStr :: Stream Int
fibStr = 0 :< (1 :< fibStr) + fibStr

{-
With sums : Σ s = 0 ≺ s + Σ s
x = 0 ≺ s + x
-}
sums :: Stream Int -> Stream Int
sums s = 0 :< (s + sums s)

fibs'' :: Stream Int
fibs'' = sums fibs' + 1

fibs :: Stream Int
fibs = 0 :< fibs'

fibs' :: Stream Int
fibs' = 1 :< (fibs + fibs')

main = print fibStr

{--
n * (n - 1) * ... * 1
--}
fact :: Int -> Int
fact 0 = 0
fact n = n * fact (n - 1)

(===) :: a -> a -> a
x === y = y
