module Y20.Day13p2 (solve20d13p2) where

{-
Given the test data
7,13,x,x,59,x,31,19

the first two pairs of (offset, id) are
(0,7) and (1,13)

so we want to solve a system of equations where
(t + 0) `mod` 7  == 0
(t + 1) `mod` 13 == 0
(t + 4) `mod` 59 == 0
(t + 6) `mod` 31 == 0
(t + 7) `mod` 19 == 0
-}
import Data.List.Split (splitOn)
import Data.Bifunctor (Bifunctor(second))
import Debug.Trace ( trace )

t :: [Char]
t = "939\n7,13,x,x,59,x,31,19"

parse :: String -> [(Int,Int)]
parse s =
  map (second read)
    $ filter (\(_,c) -> c /= "x") 
    $ zip [0,1..] 
    $ splitOn ","
    $ (!! 1)
    $ lines s 

solve20d13p2 :: IO ()
solve20d13p2 = do
  raw <- readFile "./data/20/Day13.txt"
  print $ solve $ parse raw

solve :: [(Int, Int)] -> Int
solve =
  fst . foldl loop (0, 1)
  where
    loop (base, step) (offset, i) = (base', step * i)
      where
        base' = until (\n -> (n + offset) `mod` i == 0)
                      (+ step)
                      (trace ("offset: " ++ show offset ++ "\tbase: " ++ show base ++ "\tstep: " ++ show step) base)