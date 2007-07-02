import Char

type Digit = Char

oneTwos :: [Int]
oneTwos = oneTwos 1
    where
      oneTwos x | x == 1 = 1:oneTwos 2
                | otherwise = 2:oneTwos 1

luhnChecksum :: [Digit] -> Int
luhnChecksum digits = sum (map weight (zip (reverse (map digitToInt digits)) oneTwos))
    where
      weight (x, y) = if x * y > 9 then x * y - 9 else x * y

luhn :: [Digit] -> Bool
luhn digits = rem (luhnChecksum digits) 10 == 0

lengthIs :: Int -> [Digit] -> Bool
lengthIs expected digits = length digits == expected

lengthIsOneOf :: [Int] -> [Digit] -> Bool
lengthIsOneOf ls digits = or (map (\x -> lengthIs x digits) ls)

beginsWith :: [Digit] -> [Digit] -> Bool
beginsWith expected digits = take (length expected) digits == expected

beginsWithOneOf :: [[Digit]] -> [Digit] -> Bool
beginsWithOneOf ls digits = or (map (\x -> beginsWith x digits) ls)

range :: [Digit] -> [Digit] -> [[Digit]]
range s e = map show [(read s :: Int)..(read e :: Int)]

satisfyAll :: [(a -> Bool)] -> a -> Bool
satisfyAll predicates value = and (map (\p -> p value) predicates)

guessType :: [Digit] -> [String]
guessType digits = [name | (name, discriminant) <- types, satisfyAll discriminant digits]
    where
      types = [("AMEX", [beginsWithOneOf ["34", "37"], lengthIs 15]),
               ("Discover", [beginsWith "6011", lengthIs 16]),
               ("MasterCard", [beginsWithOneOf (range "51" "55"), lengthIs 16]),
               ("Visa", [beginsWith "4", lengthIsOneOf [13,16]])]
