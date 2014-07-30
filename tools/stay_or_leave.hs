import System.Random
import Control.Applicative
import Control.Monad
import System.Environment
import Text.Printf

data JobAction = Stay | Leave
               deriving(Eq, Show)

stayOrLeave :: Int -> IO JobAction
stayOrLeave stayFactor = do
  n <- randomIO :: IO Float
  return (if n < (1 / fromIntegral stayFactor)
          then Leave
          else Stay)

main :: IO ()
main = do
  factor <- ((read :: String -> Int) . head) <$> getArgs
  decision <- stayOrLeave factor
  printf "Today's decision is %s.\n" (show decision)
