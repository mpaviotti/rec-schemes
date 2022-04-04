{-# LANGUAGE RankNTypes #-}

module Fibonacci where

import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Nat
import Data.Array.IArray

import Streams
import RecSchemes
import RecNat

{- FIBONACCI via General Recursion
   Computational complexity : Θ(2ⁿ)
-}
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

{- Fibonacci via Dynamic Programming using General Recursion -}
fibDyn :: Int -> Int
fibDyn n = table!n
  where
    table    = tabulate (0, n) fibAux
    fibAux 0 = 0
    fibAux 1 = 1
    fibAux n = table!(n - 1) + table!(n - 2)

{- Fibonacci with RSFC and mutumorphisms -}
fibDynRSFC :: Fix NatF -> Int
fibDynRSFC = proj1 . rsfcc alg
  where
    alg :: NatF (CoFree NatF (Int, Int)) -> (Int, Int)
    alg ZeroF                           = (0, 1)
    alg (SuccF (CoFree (n, m) rest))    = (m, n + m)

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

{- FIBONACCI via Streams -}

{- Fibonacci via Streams using (unsafe) general recursion -}
fibStream :: Stream Int
fibStream = 0 :< (1 :< fibStream) + fibStream

{- Fibonacci via unfolds -}
data StreamF a k = a :<< k

{-
Σ s = 0 ≺ s + Σ s
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
