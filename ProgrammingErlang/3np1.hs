distributeFairly nelements nchunk = luckyOnes ++ unluckyOnes
    where
      atLeast = (div nelements nchunk)
      remainder = (rem nelements nchunk)
      luckyOnes = (replicate remainder (atLeast + 1))
      unluckyOnes = (replicate (nchunk - remainder) atLeast)

distributeFairly' nelements nchunk = map plus (take nchunk (zip ((take r ones) ++ zeros) (repeat d)))
    where
      r = (rem nelements nchunk)
      d = (div nelements nchunk)
      zeros = repeat 0
      ones = repeat 1
      plus (a, b) = a + b

step n = step n 1
    where
      step 1 acc = acc
      step a acc | (rem a 2) == 0 = step (div a 2) (acc + 1)
                 | otherwise = step (3 * a + 1) (acc + 1)

maximum_step xs = maximum (map step xs)

main = putStrLn (show (maximum_step [1..1000000]))

