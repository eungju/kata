import Char
import Test.HUnit

type Digit = Char

oneTwos :: [Int]
oneTwos = oneTwos 1
    where
      oneTwos x | x == 1 = 1:oneTwos 2
                | otherwise = 2:oneTwos 1

luhnChecksum :: [Digit] -> Int
luhnChecksum digits = sum (map weight pairs)
    where
      pairs = zip (reverse (map digitToInt digits)) oneTwos
      weight (x, y) = if x * y > 9 then x * y - 9 else x * y

luhn :: [Digit] -> Bool
luhn digits = rem (luhnChecksum digits) 10 == 0

lengthIs :: Int -> [a] -> Bool
lengthIs expected xs = length xs == expected

beginsWith :: Eq a => [a] -> [a] -> Bool
beginsWith expected xs = take (length expected) xs == expected

range :: Int -> Int -> [[Digit]]
range s e = map show [s..e]

satisfyAny :: [a -> Bool] -> a -> Bool
satisfyAny predicates x = or (map (\p -> p x) predicates)

satisfyAll :: [a -> Bool] -> a -> Bool
satisfyAll predicates x = and (map (\p -> p x) predicates)

lengthIsOneOf :: [Int] -> [a] -> Bool
lengthIsOneOf es xs = satisfyAny (map lengthIs es) xs

beginsWithOneOf :: Eq a => [[a]] -> [a] -> Bool
beginsWithOneOf es xs = satisfyAny (map beginsWith es) xs

guessType :: [Digit] -> [String]
guessType digits = [name | (name, discriminant) <- types, discriminant digits]
    where
      types = [("AMEX", satisfyAll [beginsWithOneOf ["34", "37"], lengthIs 15]),
               ("Discover", satisfyAll [beginsWith "6011", lengthIs 16]),
               ("MasterCard", satisfyAll [beginsWithOneOf (range 51 55), lengthIs 16]),
               ("Visa", satisfyAll [beginsWith "4", lengthIsOneOf [13,16]])]

testLengthIsOneOf = TestCase (assertEqual "lengthIsOneOf [3] 123" True (lengthIsOneOf [3] "123"))

testVisa = TestCase (assertEqual "should be a visa" ["Visa"] (guessType "4417123456789112"))

tests = TestList [testLengthIsOneOf, testVisa]
