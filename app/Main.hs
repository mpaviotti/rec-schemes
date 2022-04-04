{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prelude hiding (pi, repeat, head, tail, filter, iterate, succ)


{- Algorithms -}
import Fibonacci
import Factorial
import Ackermann

main = print fibStream

(===) :: a -> a -> a
x === y = y
