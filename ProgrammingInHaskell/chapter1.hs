product' [] = 1
product' (x:xs) = x * product xs

rqsort [] = []
rqsort (x:xs) = rqsort larger ++ [x] ++ rqsort smaller
    where
      smaller = [a | a <- xs, a <= x]
      larger = [b | b <- xs, b > x]

qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
    where
      smaller = [a | a <- xs, a < x]
      larger = [b | b <- xs, b > x]
