-- |

module Reverse where

import RecSchemes

reverse :: [a] -> [a]
reverse []      = []
reverse (x:xs)  = reverse xs ++ [x]

revCat :: [a] -> [a] -> [a]
revCat ys []      = ys
revCat ys (x:xs)  = revCat (x:ys) xs

reverse' :: [a] -> [a] -> [a]
reverse' = revcat []
