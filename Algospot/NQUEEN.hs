import Control.Monad
import Control.Applicative
import Data.List

safeVertical :: [Int] -> Int -> Bool
safeVertical cs c = c `notElem` cs

safeDiagonal :: [Int] -> Int -> Bool
safeDiagonal cs c = all (uncurry (/=)) $ zip cs [c-1,c-2..0] ++ zip cs [c+1..]

safe :: [Int] -> Int -> Bool
safe cs c = safeVertical cs c && safeDiagonal cs c

solve :: Int -> [Int] -> Int
solve n cs | n == length cs = 1
           | otherwise = sum [solve n (c:cs) | c <- [0..n-1], safe cs c]

countSolutions :: Int -> Int
countSolutions n = solve n []

main :: IO ()
main = do
  c <- read <$> getLine :: IO Int
  forM_ [1..c] $ \_ -> do
    n <- read <$> getLine :: IO Int
    print $ countSolutions n
