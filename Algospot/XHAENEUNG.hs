import Control.Monad
import Control.Applicative
import Data.Char
import Data.List

splitOnSpace [] = []
splitOnSpace x = a:splitOnSpace (dropWhile isSpace b)
  where (a, b) = break isSpace x

fuzzyMatch a b = sort a == sort b

numberWords = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]

intToWord i = if i < 0 || i > 10 then "" else numberWords !! i

wordToInt w = case elemIndex w numberWords of Just i -> i

main :: IO ()
main = do
  t <- read <$> getLine :: IO Integer
  forM_ [1..t] $ \_ -> do
    (a:op:b:"=":c:[]) <- splitOnSpace <$> getLine :: IO [String]
    let r = intToWord $ (case op of "+" -> (+)
                                    "-" -> (-)
                                    "*" -> (*)) (wordToInt a) (wordToInt b)
    putStrLn $ if fuzzyMatch r c then "Yes" else "No"
