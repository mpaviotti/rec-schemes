module Factorial where

{--
n * (n - 1) * ... * 1
--}
fact :: Int -> Int
fact 0 = 0
fact n = n * fact (n - 1)
