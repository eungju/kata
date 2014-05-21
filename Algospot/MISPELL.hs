import Control.Monad
import Control.Applicative
import Data.Char

main :: IO ()
main = do
  n <- read <$> getLine :: IO Integer
  forM_ [1..n] $ \i -> do
    s <- getLine
    let m = read $ takeWhile (not . isSpace) s :: Int
        w = dropWhile isSpace (dropWhile (not . isSpace) s)
        (a, b) = splitAt (m - 1) w
    putStrLn $ show i ++ " " ++ a ++ drop 1 b
