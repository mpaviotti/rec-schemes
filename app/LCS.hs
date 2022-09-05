module LCS where


{-- The longest Common Subsequence : Naive Version --}
lcs :: Ord a => [a] -> [a] -> Int
lcs [] _ = 0
lcs _ [] = 0
lcs xss@(x:xs) yss@(y:ys)
  | x == y     = lcs xs ys
  | otherwise  = max (lcs xs yss) (lcs xss ys)
