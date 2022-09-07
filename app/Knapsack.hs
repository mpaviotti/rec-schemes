module Knapsack where

import RecSchemes
import RecNat

maximun :: [Int] -> Int
maximun []     = 0
maximun (x:xs) = if x < maximun xs then maximun xs else x

knapsack :: [(Int, Int)] -> Int -> Int
knapsack wvs tweight = maximun [v + knapsack wvs (tweight - w) | (w,v) <- wvs, w <= tweight]

{- with dynamic programming-}
dynKnapsack :: [(Int, Int)] -> Int -> Int
dynKnapsack wvs tweight = table!!tweight where
  table = [ks i | i <- [0..tweight]]
  ks i = maximun [v + table!!(tweight-w) | (w,v) <- wvs, w <= tweight]


knapsackHisto :: [(Fix NatF, Fix NatF)] -> Fix NatF -> Fix NatF
knapsackHisto wvs = histo alg
  where
    alg :: NatF (CoFree NatF (Fix NatF)) -> Fix NatF
    alg ZeroF            = zero
    alg (SuccF table)    = maximun [v + u | (w,v) <- wvs, Just u <- [lookup table w] ]
    lookup :: CoFree NatF (Fix NatF) -> Fix NatF -> Maybe (Fix NatF)
    lookup (x :< _) (In ZeroF)              = Just x
    lookup (x :< ZeroF) (In (SuccF _))          = Nothing
    lookup (_ :< SuccF table) (In (SuccF n))    = lookup table n
    maximun :: [Fix NatF] -> Fix NatF
    maximun [] = zero
    maximun (x:xs) = if x < maximun xs then maximun xs else x
