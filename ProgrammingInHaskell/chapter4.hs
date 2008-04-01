halve xs | even (length xs) = (take n xs, drop n xs)
         where
           n = length xs `div` 2

or' a b = if a && b then
              True
          else
              False

or'' a b = if a then
               b
           else
               False

safetail'a xs = if null xs then [] else tail xs
safetail'b xs | null xs = []
              | otherwise = tail xs
safetail'c [] = []
safetail'c xs = tail xs

or'1 True True = True
or'1 True False = True
or'1 False True = True
or'1 False False = False

or'2 False False = False
or'2 _ _ = True

or'3 True _ = True
or'3 _ True = True
or'3 _ _ = False

or'4 True _ = True
or'4 False b = b
