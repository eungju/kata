import Control.Monad
import Control.Applicative

main :: IO ()
main = do
  n <- read <$> getLine :: IO Integer
  forM_ [1..n] $ \_ -> do
    s <- getLine
    let (evens, odds) = split s [] []
    putStrLn $ evens ++ odds
    where
      split [] e o = (reverse e, reverse o)
      split (a:[]) e o = split [] (a:e) o
      split (a:b:xs) e o = split xs (a:e) (b:o)
