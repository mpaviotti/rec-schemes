module Knapsack where

import RecSchemes

maximun :: [Int] -> Int1
maximun []     = 0
maximun (x:xs) = if x < maximun xs then maximun xs else x

knapsack :: [(Int, Int)] -> Int -> Int
knapsack wvs tweight = maximun [v + knapsack wvs (tweight - w) | (w,v) <- wvs, w <= tweight]

{- with dynamic programming-}
dynKnapsack :: [(Int, Int)] -> Int -> Int
dynKnapsack wvs tweight = table!!tweight where
  table = [ks i | i <- [0..tweight]]
  ks i = maximun [v + table!!(tweight-w) | (w,v) <- wvs, w <= tweight]
