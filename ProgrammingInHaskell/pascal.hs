a :: Int -> Int -> Int
a n k | k == 1 = 1
      | n == k = 1
      | otherwise = a (n - 1) (k - 1) + a (n - 1) k

f :: Int -> Int
f n = product [1..n] 

(!) :: Int -> Int -> Int
n ! k = div (f n) ((f k) * (f (n - k))) 

a' :: Int -> Int -> Int
a' n k = (n - 1) ! (k - 1)

triangle :: Int -> [[Int]]
triangle h = map (\n -> map (a' n) [1..n]) [1..h]

