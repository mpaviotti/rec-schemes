{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module RecNat where

import RecSchemes
import Prelude hiding (succ)

{- Natural Numbers -}
data NatF k = ZeroF | SuccF k deriving Functor

instance (Num (Fix NatF)) where
  (+) :: Fix NatF -> Fix NatF -> Fix NatF
  x + y = foldParam palg (x, y)
    where
      palg :: (NatF (Fix NatF -> Fix NatF), Fix NatF) -> Fix NatF
      palg (ZeroF, y')    = y'
      palg (SuccF x', y') = In (SuccF (x' y'))

  (*) :: Fix NatF -> Fix NatF -> Fix NatF
  x * y = foldParam palg (x, y)
    where
      palg :: (NatF (Fix NatF -> Fix NatF), Fix NatF) -> Fix NatF
      palg (ZeroF,   y')    = zero
      palg (SuccF f, y')    = f y' + y'

  (-) :: Fix NatF -> Fix NatF -> Fix NatF
  x - y = paramWithParam ppalg (x, y)
    where
      ppalg :: (NatF (Fix NatF -> (Fix NatF, Fix NatF)), Fix NatF) -> Fix NatF
      ppalg (ZeroF,   m)              = zero
      ppalg (SuccF f, In ZeroF)       = let (x, _) = f zero in x
      ppalg (SuccF f, In (SuccF m'))  = let (_, x) = f m' in x

  abs :: Fix NatF -> Fix NatF
  abs = id

  negate :: Fix NatF -> Fix NatF
  negate = id

  fromInteger :: Integer -> Fix NatF
  fromInteger 0         = zero
  fromInteger n | n > 0 = succ (fromInteger (n - 1))
  fromInteger n | n < 0 = error "undefined: negative integer is not natural."

paramWithParam :: (Functor f) =>  ((f (p -> (Fix f, y)), p) -> y) -> (Fix f, p) -> y
paramWithParam alg (In x, p) = alg ((fmap (\x' -> (\p -> (In x, (paramWithParam alg (x', p))))) x), p)

remainder :: Fix NatF -> Fix NatF -> Fix NatF
remainder x y = foldParam palg (x, y)
  where
    palg :: (NatF (Fix NatF -> Fix NatF), Fix NatF) -> Fix NatF
    palg (ZeroF,   m) = zero
    palg (SuccF f, m) = if f m == (m - one) then zero else f m + one

instance (Eq (Fix NatF)) where
  (==) :: Fix NatF -> Fix NatF -> Bool
  x == y = foldParam palg (x, y)
    where
      palg :: (NatF (Fix NatF -> Bool), Fix NatF) -> Bool
      palg (ZeroF, (In ZeroF))         = True
      palg (ZeroF, (In (SuccF m')))    = False
      palg (SuccF f, (In ZeroF))       = False
      palg (SuccF f, (In (SuccF m')))  = f m'

instance (Show (Fix NatF)) where
  show :: Fix NatF -> String
  show n = show (toInt n)
    where
      toInt :: Fix NatF -> Int
      toInt = fold alg
      alg :: NatF Int -> Int
      alg ZeroF = 0
      alg (SuccF n') = n' + 1

succ :: Fix NatF -> Fix NatF
succ n = In (SuccF n)

zero :: Fix NatF
zero  = In ZeroF
one   = succ zero
two   = succ one
three = succ two
four  = succ three
five  = succ four
six   = succ six
seven = succ six
eight = succ seven
nine  = succ eight
ten   = succ nine
