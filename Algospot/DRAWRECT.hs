data Point = Point Int Int deriving (Show, Read)

oddPos a b c
  | a == b = c
  | b == c = a
  | otherwise = b

lastPoint :: Point -> Point -> Point -> Point
lastPoint (Point x1 y1) (Point x2 y2) (Point x3 y3) = Point (oddPos x1 x2 x3) (oddPos y1 y2 y3)

solveCase :: Int -> IO ()
solveCase i = do
  p1 <- getLine
  p2 <- getLine
  p3 <- getLine
  putStrLn $ drop 6 $ show $ lastPoint (read ("Point " ++ p1)) (read ("Point " ++ p2)) (read ("Point " ++ p3))

main :: IO ()
main = do
  input <- getLine
  mapM_ solveCase [1..(read input)]
