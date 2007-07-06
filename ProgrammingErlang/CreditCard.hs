import Char
import Test.HUnit

type Digit = Char

oneTwos :: [Int]
oneTwos = oneTwos 1
    where
      oneTwos 1 = 1:oneTwos 2
      oneTwos 2 = 2:oneTwos 1

luhnChecksum :: [Digit] -> Int
luhnChecksum digits = sum (map weight pairs)
    where
      pairs = zip (reverse (map digitToInt digits)) oneTwos
      weight (x, y) = if x * y > 9 then x * y - 9 else x * y

luhn :: [Digit] -> Bool
luhn digits = rem (luhnChecksum digits) 10 == 0

lengthIs :: Int -> [a] -> Bool
lengthIs expected xs = length xs == expected

beginWith :: Eq a => [a] -> [a] -> Bool
beginWith expected xs = take (length expected) xs == expected

satisfyAny :: [a -> Bool] -> a -> Bool
satisfyAny predicates x = or (map (\p -> p x) predicates)

satisfyAll :: [a -> Bool] -> a -> Bool
satisfyAll predicates x = and (map (\p -> p x) predicates)

lengthIsOneOf :: [Int] -> [a] -> Bool
lengthIsOneOf es xs = satisfyAny (map lengthIs es) xs

beginWithOneOf :: Eq a => [[a]] -> [a] -> Bool
beginWithOneOf es xs = satisfyAny (map beginWith es) xs

beginWithRange :: [Digit] -> [Digit] -> [Digit]-> Bool
beginWithRange s e xs = beginWithOneOf (range s e) xs
    where
      range :: [Digit] -> [Digit] -> [[Digit]]
      range s e = map show [(read s :: Int)..(read e :: Int)]

guessType :: [Digit] -> [String]
guessType digits = [name | (name, discriminant) <- types, discriminant digits]
    where
      types = [("AMEX", satisfyAll [beginWithOneOf ["34", "37"], lengthIs 15]),
               ("Discover", satisfyAll [beginWith "6011", lengthIs 16]),
               ("MasterCard", satisfyAll [beginWithRange "51" "55", lengthIs 16]),
               ("Visa", satisfyAll [beginWith "4", lengthIsOneOf [13,16]])]

tests = test [
         "1" ~: take 1 oneTwos ~?= [1],
         "12" ~: take 2 oneTwos ~?= [1,2],
         "121" ~: take 3 oneTwos ~?= [1,2,1],
         "Luhn checksum" ~: luhnChecksum "1" ~?= 1,
         "Luhn checksum" ~: luhnChecksum "12" ~?= 4,
         "Luhn checksum" ~: luhnChecksum "80" ~?= 7,
         "Luhn checksum is valid" ~: luhn "34" ~?= True,
         "Luhn checksum is invalid" ~: luhn "123" ~?= False,
         "Length is" ~: lengthIs 3 "124" ~?= True,
         "Length is not" ~: lengthIs 4 "124" ~?= False,
         "Begins with" ~: beginWith "432" "4321" ~?= True,
         "Doesn't begin with" ~: beginWith "123" "124" ~?= False,
         "Satisfies any" ~: satisfyAny [t_p, f_p] "1" ~?= True,
         "Doesn't satisfy any" ~: satisfyAny [f_p, f_p] "1" ~?= False,
         "Satisfies all" ~: satisfyAll [t_p, t_p] "1" ~?= True,
         "Doesn't satisfy all" ~: satisfyAll [t_p, f_p] "1" ~?= False,
         "It's Visa" ~: guessType "4417123456789112" ~?= ["Visa"]
        ]
    where
      t_p x = True
      f_p x = False
