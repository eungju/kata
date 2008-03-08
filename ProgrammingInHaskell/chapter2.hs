n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last'1 [x] = x
last'1 (x:xs) = last'1 xs

init'1 [x] = []
init'1 (x:xs) = (x:(init'1 xs))

init'2 = reverse . tail . reverse