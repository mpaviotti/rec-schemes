{-# LANGUAGE InstanceSigs #-}
module Streams where

import Prelude hiding (pi, repeat, head, tail, filter, iterate, succ)
import Control.Applicative (liftA2)
import Data.List hiding (repeat)

{- Streams of Fibonacci numbers -}
data Stream a = a :< Stream a

repeat :: a -> Stream a
repeat x = x :< repeat x

list :: Int -> Stream a -> [a]
list 0 _ = []
list n (x :< xs) = x : list (n - 1) xs

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

infix 3 :<

instance Show a => Show (Stream a) where
  show xs = (show (list 10 xs))

class Functor f => Multplicative f where
  unit :: f ()
  (><) :: f a -> f b -> f (a, b)

instance Multplicative Stream where
  unit = repeat ()
  (x :< xs) >< (y :< ys) = (x, y) :< (xs >< ys)
