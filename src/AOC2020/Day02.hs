module AOC2020.Day02 where

import Data.List.Split ( splitOn )

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

-- | Solve the puzzle using both alorithms
solve :: String -> (Int,Int)
solve ss =
  let anss = map isLegal $ lines ss in
  ( length $ filter (==True) $ map fst anss
  , length $ filter (==True) $ map snd anss)

-- | Solve for data in a file return solution for bolth algos
solveFor :: String -> IO (Int,Int)
solveFor filename = do 
  raw <- readFile filename
  return $ solve raw
  