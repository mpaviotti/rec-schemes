{-# LANGUAGE DeriveFunctor #-}


module QuickSort where

import RecSchemes

partition :: (Ord a) => [a] -> ([a], a, [a])
partition (x:xs) = ([y | y <- xs, y < x], x, [z | z <- xs, z >= x])

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort lst = let (ls, x, rs) = partition lst in
                  quicksort ls ++ [x] ++ quicksort rs

data TreeF a x = TreeF x a x deriving Functor

quicksortHylo :: (Ord a) => [a] -> [a]
quicksortHylo = hylo alg coalg
  where
    coalg :: [a] -> TreeF a [a]
    coalg (x:xs) = TreeF [y | y <- xs, y < x] x [z | z <- xs, z >= x]
    alg :: TreeF a [a] -> [a]
    alg (TreeF ls x rs) = ls ++ [x] ++ rs
