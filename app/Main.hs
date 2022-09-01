{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding (foldr, foldl, pi, repeat, head, tail, filter, iterate, succ)

{- Algorithms -}
import Fibonacci
import Factorial
import Ackermann
import RecNat

main :: IO ()
main = do {
  print fibStream;
  print $ "Mutu Factorial 3: " ++ show (factM 3);
  print $ "Mutu Factorial 3 with Recursion Schemes: " ++ show (factMutu three);
  }


(===) :: a -> a -> a
x === y = y
