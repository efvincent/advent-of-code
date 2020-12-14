module Y20.Day06 (solve20d6p1, solve20d6p2) where

import qualified Data.Set as Set
import Data.List (intersect)
import Data.List.Split (splitOn)

fn20d6 :: String
fn20d6 = "./data/20/Day06.txt"

solve20d6p1 :: IO ()
solve20d6p1 = do
  raw <- readFile fn20d6
  let groups = map (concat . splitOn "\n") $ splitOn "\n\n" raw 
  print  $ sum $ map (length . Set.fromList) groups 

solve20d6p2 :: IO ()
solve20d6p2 = do
  raw <- readFile fn20d6
  let groups = map
                (splitOn "\n")
                (splitOn "\n\n" raw)
  print $ sum $ map length $ filter (/= "") $ map (foldl1 intersect) groups
