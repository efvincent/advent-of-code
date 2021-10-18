module Y20.Day05 (solve20d5p1,solve20d5p2,fn20d5) where
import Data.List (sort)

fn20d5 :: [Char]
fn20d5 = "./data/20/Day05.txt"

decode :: [Char] -> [Int]
decode = map (\c -> if c == 'F' || c == 'L' then 0 else 1)

binToInt :: [Int] -> Int
binToInt = foldl (\acc n -> 2 * acc + n) 0

solve20d5p1 :: IO ()
solve20d5p1 = do
  raw <- readFile fn20d5
  print $ maximum $ map (binToInt . decode) $ lines raw

findGap :: [Int] -> Int
findGap [] = 0
findGap [_] = 0
findGap (x:y:_) | y - x == 2 = y-1 
findGap (_:y:xs) = findGap (y:xs)

solve20d5p2 :: IO ()
solve20d5p2 = do
  raw <- readFile fn20d5
  print $ findGap $ sort $ map (binToInt . decode) $ lines raw 