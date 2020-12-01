module Util where

import Data.List.Split ( splitOn )

-- | Runs a single test, by checking 'control' is equal to 'candidate'
-- Returns 'Either' where 'Right ()' is success, and 'Left msg' is the message supplied
-- in a failure case
check :: Eq a => String -> a -> a -> Either String ()
check msg control candidate =
  if control == candidate then Right () else Left msg

-- | Given a filename of a text file with one Int per line, returns 'IO [Int]' 
fileToIntList :: FilePath -> IO [Int]
fileToIntList filename = do
  raw <- readFile filename
  return $ map read $ words raw

-- | Given a file with a single line of comma separated Ints, return [Int]
fileOfCSVToIntList :: FilePath -> IO [Int]
fileOfCSVToIntList filename = do
  raw <- readFile filename
  return $ map read $ splitOn "," raw

-- | Given a filename of lines where each line is a comma separated list of strings,
-- return [[String]]
fileOfLinesOfCSVStringsToLists :: FilePath -> IO [[String]]
fileOfLinesOfCSVStringsToLists filename = do
  raw <- readFile filename
  return $ map (splitOn ",") $ lines raw