import Control.Monad
import Control.Applicative
import Data.List

onVertical :: [Int] -> Int -> Bool
onVertical cs c = elem c cs

onDiagonal :: [Int] -> Int -> Bool
onDiagonal cs c = any (uncurry (==)) $ zip cs [c-1,c-2..0] ++ zip cs [c+1..]

onAttack :: [Int] -> Int -> Bool
onAttack cs c = onVertical cs c || onDiagonal cs c

solve :: Int -> Int -> [Int] -> Int
solve n m cs | n == m = 1
             | otherwise = sum [solve n (m + 1) (c:cs) | c <- [0..n-1], not (onAttack cs c)]

countSolutions :: Int -> Int
countSolutions n = solve n 0 []

main :: IO ()
main = do
  c <- read <$> getLine :: IO Int
  forM_ [1..c] $ \_ -> do
    n <- read <$> getLine :: IO Int
    print $ countSolutions n
