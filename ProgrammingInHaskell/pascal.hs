triangle :: Int -> [[Int]]
triangle n = reverse foldl (\_ acc -> next ((head acc):acc)) [[1]] [1..n]
    where
      next :: [Int] -> [Int]
      next s = foldl (\a (b:bs) -> (a:a+b:bs)) [0] s
