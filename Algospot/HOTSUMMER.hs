import Control.Monad
import Control.Applicative
import Data.Char

main :: IO ()
main = do
  t <- read <$> getLine :: IO Integer
  forM_ [1..t] $ \_ -> do
    w <- read <$> getLine :: IO Integer
    xs <- map read . splitOnSpace <$> getLine :: IO [Integer]
    putStrLn $ if w >= sum xs then "YES" else "NO"
    where
      splitOnSpace [] = []
      splitOnSpace x = a:splitOnSpace (dropWhile isSpace b)
        where (a, b) = break isSpace x
