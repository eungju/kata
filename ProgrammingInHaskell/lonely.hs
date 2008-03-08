import List

subset :: [a] -> [[a]]
subset [] = [[]]
subset (x:xs) = [x:ys | ys <- subset xs] ++ (subset xs)

combination :: [a] -> Int -> [[a]]
combination xs i = [ys | ys <- subset xs, length ys == i]

permutation :: Eq a => [a] -> [[a]]
permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- (permutation (delete x xs))]

--[xs | xs <- permutation [1..5], not (or (zipWith (==) [1..5] xs))]
