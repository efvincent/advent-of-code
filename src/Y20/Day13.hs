module Y20.Day13 (solve20d13p1) where

import Data.Ord ( comparing ) 
import Data.List.Split ( splitOn )
import Data.List.Extra ( minimumBy )

t = "939\n7,13,x,x,59,x,31,19"

fn20d13 :: [Char]
fn20d13 = "./Data/20/Day13.txt"

bestTime :: (Ord b, Num b) => b -> b -> (b, b)
bestTime target busId = 
  let time = head $ filter (>= target) $ iterate (+ busId) 0
  in (busId, time)

parse1 :: String -> (Int, [Int])
parse1 s =
  (read t,busIds)
  where
    [t,bs] = lines s
    busIds = map read $ filter (/= "x") $ splitOn "," bs

solve20d13p1 :: [Char] -> Int
solve20d13p1 s =
  id * (time - target)
  where
    (target, ids) = parse1 s
    (id, time) = minimumBy (comparing snd) $ map (bestTime target) ids
