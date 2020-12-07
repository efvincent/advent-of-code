module Y20.Day06 where

import qualified Data.Set as Set
import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.Function ( (&) ) 
import Flow ((|>))

filename6 :: String
filename6 = "./data/20/Day06.txt"

solve61 :: IO ()
solve61 = do
  raw <- readFile filename6
  let groups = map (concat . splitOn "\n") $ splitOn "\n\n" raw 
  print  $ sum $ map (length . Set.fromList) groups 

solve61flow :: IO ()
solve61flow = do
  raw <- readFile filename6
  splitOn "\n\n" raw 
    & map (concat . splitOn "\n") 
    & map (length . Set.fromList) 
    & sum 
    & print

solve62 :: IO ()
solve62 = do
  raw <- readFile filename6
  let groups = 
        map (splitOn "\n") (splitOn "\n\n" raw) 
        |> map (foldl1 intersect) 
        |> filter (/= "") 
        |> map length 
        |> sum
  print groups
