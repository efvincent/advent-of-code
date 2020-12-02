module AOC2020.Day02 where

import Data.List.Split ( splitOn )

data Part = PART1 | PART2

-- | Checks for a legal string using two algorithms, returns tuple of two
-- bool results.
isLegal :: String -> (Bool,Bool)
isLegal s =
  (mn <= count && count <= mx, p1 `xor` p2)
  where
    [rule,test] = splitOn ": " s
    [nums, c:_] = splitOn " " rule    
    [mn, mx] = map read $ splitOn "-" nums
    count = length $ filter (== c) test
    p1 = test!!(mn-1) == c
    p2 = test!!(mx-1) == c

-- | Quicker to type xor than look up what package (if any) contains it
xor :: Bool -> Bool -> Bool
xor x y = (x && not y ) || (not x && y)

-- | Solve the puzzle, counting the number of legal results where each line
-- in the string is a test. Use algorithm determined by `Part`
solve :: Part -> String -> Int
solve p ss =
  let idx = case p of PART1 -> fst; PART2 -> snd in
  length $ filter (==True) $ map (idx . isLegal) $ lines ss

-- | Solve for data in a file
solveFor :: Part -> String -> IO Int
solveFor part filename = do 
  raw <- readFile filename
  return $ solve part raw
  