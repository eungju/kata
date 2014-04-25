#!/usr/bin/env runghc

import qualified Codec.Binary.Base32 as Base32
import Codec.Utils (i2osp, fromTwosComp)
import qualified Control.Arrow as Arrow
import Data.Bits
import Data.Char
import Data.Functor
import Data.HMAC
import Data.List.Split
import Data.Maybe
import Data.Time.Clock.POSIX
import Data.Word
import Network.URI
import System.Directory
import System.FilePath
import Text.Printf

parseParameter :: String -> (String, String)
parseParameter = Arrow.second (dropWhile (== '=')) . break (== '=')

parseQuery :: String -> [(String, String)]
parseQuery = map parseParameter . split (dropDelims $ oneOf "&") . dropWhile (== '?')

data Account = Account { username :: String, secret :: String
                       --, period :: Int, digits :: Int, algorithm :: String
                       } deriving (Eq, Show, Read)

parseAccount :: URI -> Maybe Account
parseAccount uri = Account (dropWhile (== '/') $ uriPath uri)
                   <$> map toUpper <$> lookup "secret" (parseQuery (uriQuery uri))

generateCode :: Account -> POSIXTime -> (String, Int)
generateCode account t = (pad code (6 :: Int), timeout)
  where
    key = fromJust $ Base32.decode $ secret account :: [Word8]
    timeout = 30 - (round t `mod` 30) :: Int
    message = i2osp 8 (round t `div` 30 :: Word64) :: [Word8]
    hash = hmac_sha1 key message :: [Word8]
    offset = fromIntegral $ last hash .&. 0x0F :: Int
    truncatedHash = fromTwosComp $ take 4 $ drop offset hash :: Word32
    code = truncatedHash .&. 0x7FFFFFFF :: Word32
    pad i n = printf (printf "%%0%dd" n) (rem i (10 ^ n)) :: String

main :: IO ()
main = do
  t <- getPOSIXTime
  dataPath <- (`combine` ".otp") <$> getHomeDirectory
  lines <$> readFile dataPath >>=
    mapM_ (\line -> case parseURI line >>= parseAccount of
              Just account -> do
                let (code, timeout) = generateCode account t
                printf "%s %02ds %s \n" code timeout (username account)
              Nothing -> return ())
