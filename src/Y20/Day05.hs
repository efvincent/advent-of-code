module Y20.Day05 where
import qualified Data.Set as Set

filename :: [Char]
filename = "./data/20/Day05.txt"

{-
not the original approach, switched after realizing the string was just
binary. This is so much nicer.
-}

decode :: [Char] -> [Int]
decode = map (\c -> if c == 'F' || c == 'L' then 0 else 1)

binToInt :: [Int] -> Int
binToInt = foldl (\acc n -> 2 * acc + n) 0
 
solve5 :: IO ()
solve5 = do
  raw <- readFile filename
  print $ maximum $ map (binToInt . decode) $ lines raw

{-
part 2
front and rear rows don't count... so:
1. generate a set of codes from raw data
2. remove the codes from first and last rows
3. scan the remaining list in order until you find consecutive seats with a gap. 
  the gap is the answer
-}

findGap :: [Int] -> Int
findGap [] = 0
findGap (x:y:_) | y - x == 2 = y-1 
findGap (_:y:xs) = findGap (y:xs)

solve5b :: IO ()
solve5b = do
  raw <- readFile filename
  let ordered = 
        Set.toAscList
        $ foldr Set.delete (Set.fromList $ map (binToInt . decode) $ lines raw)
        $ [0..7] ++ [127 * 8 + c | c <- [0..7]] in
    print $ findGap ordered
