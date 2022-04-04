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
zero  = In ZeroF
one   = succ zero
two   = succ one
three = succ two
four  = succ three
