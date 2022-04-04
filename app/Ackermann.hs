
module Ackermann where

import Prelude hiding (succ)
import Data.Nat
import RecSchemes
import RecNat

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
