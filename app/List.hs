{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module List where

import Prelude hiding (reverse, concat)

import Data.Functor
import RecSchemes

{- Standard implementation of Reverse and Accumulating Reverse -}
reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

revCat :: [a] -> [a] -> [a]
revCat ys []      = ys
revCat ys (x:xs)  = revCat (x:ys) xs

reverse' :: [a] -> [a]
reverse' = revCat []

data ListF a k = Nil | Cons a k deriving Functor

nil :: Fix (ListF a)
nil = In Nil

cons :: a -> Fix (ListF a) -> Fix (ListF a)
cons x xs = In (Cons x xs)

concat :: Fix (ListF a) -> Fix (ListF a) -> Fix (ListF a)
concat = curry (foldParam (\case {(Nil, p) -> p; (Cons x f, p) -> cons x (f p) }))


reverseFold :: Fix (ListF a) -> Fix (ListF a)
reverseFold = fold (\case { Nil -> nil ; Cons x r -> concat r (cons x nil) })
