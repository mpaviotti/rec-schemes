module Factorial where

import RecSchemes
import RecNat

{--
n * (n - 1) * ... * 1
--}
fact :: Int -> Int
fact 0 = 0
fact n = n * fact (n - 1)

fact' :: Fix NatF -> Fix NatF
fact' = para alg
  where
    alg :: NatF (Fix NatF, Fix NatF) -> Fix NatF
    alg ZeroF          = zero
    alg (SuccF (n, r)) = n * r
