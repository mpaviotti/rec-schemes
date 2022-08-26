module Factorial where

import RecSchemes
import RecNat


{--
n * (n - 1) * ... * 1
--}
fact :: Int -> Int
fact 0 = 0
fact n = n * fact (n - 1)

factM :: Int -> Int
factM 0 = 1
factM n = (1 + factM' (n - 1)) * factM (n - 1)

factM' :: Int -> Int
factM'  = id

factMutu :: Fix NatF -> (Fix NatF, Fix NatF)
factMutu = mutu algLeft algRight
  where
    algLeft :: NatF (Fix NatF, Fix NatF) -> Fix NatF
    algLeft ZeroF             = zero
    algLeft (SuccF (n, _))   = n + one
    algRight :: NatF (Fix NatF, Fix NatF) -> Fix NatF
    algRight ZeroF            = one
    algRight (SuccF (n, fn))   = (n + one) * fn

fact' :: Fix NatF -> Fix NatF
fact' = para alg
  where
    alg :: NatF (Fix NatF, Fix NatF) -> Fix NatF
    alg ZeroF          = zero
    alg (SuccF (n, r)) = n * r
