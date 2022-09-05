
module LIS where 

import Data.List

sublists :: [a] -> [[a]]
sublists []     = [[]]
sublists (x:xs) = [x:ys | ys <- sublists xs] ++ sublists xs

maximun :: Ord a => [a] -> a
maximun [x] = x
maximun (x:xs) = if x > maximun xs then x else maximun xs

verifyOrder :: Ord a => [a] -> Bool
verifyOrder [] = True
verifyOrder [x] = True
verifyOrder (x:y:xs) = x <= y && (verifyOrder (y:xs))

{-- The longest Increasing Sequence : Naive Version --}
lis :: Ord a => [a] -> [a]
lis []     = []
lis ys@(x:xs) = snd . maximunI . map (\x -> (length x, x))  . (filter verifyOrder) . sublists $ ys
  where
    maximunI :: [(Int, a)] -> (Int, a)
    maximunI [x]      = x
    maximunI ((n, xs):xxs) = let r = maximunI xxs in if n > fst r then (n, xs) else r

{--
 Smarter version
 Idea: LIS of (x:xs) is a subsequence in xs
       or a subsequence in xs with x at the beginning

       in the latter case the LIS must have a tail that itself is also an LIS

Example. [-4,-3,-5, 4, 2, 3, 9]
if we are at (x@-5: xs@[4,3,9])
then adding -5 forbids -3 and -4 to be added next
so we have to consider both -5, 2, 3, 9 which has length 4
and [2, 3,9] to which we can add later -4, -3 which make it length 5
--}

lis' :: (Ord a) => [a] -> (Int, Int)
lis' []     = (0,0)
lis' (x:xs) = (1 + maximun [fst . lis' $ sub |  sub <- tails xs, x < head sub || null sub], snd . lis' $ xs )
