#!/usr/bin/env runghc

import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import System.Directory
import System.FilePath

data ELPA = ELPA FilePath

defaultELPA :: IO ELPA
defaultELPA = do
  home <- getHomeDirectory
  return $ ELPA (combine home ".emacs.d/elpa")

getInstalled :: ELPA -> IO [FilePath]
getInstalled (ELPA path) = sort . filter (not . isPrefixOf ".") <$> getDirectoryContents path

takeObsolete :: [FilePath] -> [FilePath]
takeObsolete packages = concatMap (tail . sortBy (flip compare)) $ filter ((>= 2) . length) $ groupBy ((==) `on` takeName) packages
  where takeName path = reverse . dropWhile (/= '-') $ reverse path

getObsolete :: ELPA -> IO [FilePath]
getObsolete elpa = takeObsolete <$> getInstalled elpa

main :: IO ()
main = do
  elpa <- defaultELPA
  outdated <- getObsolete elpa
  forM_ outdated putStrLn
