import Data.Bits

convertEndian :: Integer -> Integer
convertEndian x = foldr1 (.|.) $ map (\i -> shiftL (shiftR x i .&. 0xFF) (24 - i)) [0,8..24]

convert :: Int -> IO ()
convert i = do
  input <- getLine
  print $ convertEndian (read input)

main :: IO ()
main = do
  input <- getLine
  mapM_ convert [1..(read input)]
