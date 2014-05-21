import Control.Monad
import Control.Applicative
import Data.Char
import Text.Printf

toPound x = x * 2.2046
toKilogram x = x * 0.4536

toGallon x = x * 0.2642
toLiter x = x * 3.7854

main :: IO ()
main = do
  n <- read <$> getLine :: IO Integer
  forM_ [1..n] $ \i -> do
    s <- getLine
    let x = read $ takeWhile (not . isSpace) s :: Float
        u = dropWhile isSpace (dropWhile (not . isSpace) s)
        (y, yu) = case u of "kg" -> (toPound x, "lb")
                            "lb" -> (toKilogram x, "kg")
                            "l" -> (toGallon x, "g")
                            "g" -> (toLiter x, "l")
    putStrLn $ printf "%d %.4f %s" i y yu
