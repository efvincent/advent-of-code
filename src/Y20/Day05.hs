module Y20.Day05 where
import Data.List (sort)

filename :: [Char]
filename = "./data/20/Day05.txt"

decode :: [Char] -> [Int]
decode = map (\c -> if c == 'F' || c == 'L' then 0 else 1)

binToInt :: [Int] -> Int
binToInt = foldl (\acc n -> 2 * acc + n) 0

solve5 :: IO ()
solve5 = do
  raw <- readFile filename
  print $ maximum $ map (binToInt . decode) $ lines raw

findGap :: [Int] -> Int
findGap [] = 0
findGap (x:y:_) | y - x == 2 = y-1 
findGap (_:y:xs) = findGap (y:xs)

solve5b :: IO ()
solve5b = do
  raw <- readFile filename
  print $ findGap $ sort $ map (binToInt . decode) $ lines raw 