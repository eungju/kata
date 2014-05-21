main :: IO ()
main = do
  input <- getLine
  mapM_ putStrLn (replicate (read input) "Hello Algospot!")
