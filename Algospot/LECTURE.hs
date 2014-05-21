import Control.Monad
import Control.Applicative
import Data.List

main :: IO ()
main = do
  n <- read <$> getLine :: IO Integer
  forM_ [1..n] $ \_ -> do
    s <- getLine
    putStrLn $ concat $ sort $ split s
    where
      split [] = []
      split (a:b:xs) = [a, b]:split xs
