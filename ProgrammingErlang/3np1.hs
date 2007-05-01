distributeFairly nelements nchunk = luckyOnes ++ unluckyOnes
    where
      atLeast = (div nelements nchunk)
      remainder = (rem nelements nchunk)
      luckyOnes = (replicate remainder (atLeast + 1))
      unluckyOnes = (replicate (nchunk - remainder) atLeast)

d2 nelements nchunk = map plus (take nchunk (zip ((take r ones) ++ zeros) (repeat d)))
    where
      r = (rem nelements nchunk)
      d = (div nelements nchunk)
      zeros = repeat 0
      ones = repeat 1
      plus (a, b) = a + b

