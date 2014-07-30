import System.Random
import Control.Applicative
import System.Environment
import Text.Printf
import Data.Char

data Decision = Stay | Leave
              deriving(Eq, Show)

stayOrLeave :: Int -> IO Decision
stayOrLeave stayDays = do
  n <- randomIO :: IO Float
  return (if n < (1 / fromIntegral stayDays)
          then Leave
          else Stay)

main :: IO ()
main = do
  factor <- ((read :: String -> Int) . head) <$> getArgs
  decision <- stayOrLeave factor
  printf "You should %s.\n" (map toLower $ show decision)
