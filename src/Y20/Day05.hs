module Y20.Day05 where
import qualified Data.Set as Set

data Axis = ROW | COL

filename :: [Char]
filename = "./data/20/Day05.txt"

stepRow :: [Char] -> Int -> Int -> Int
stepRow ['F'] low _ = low
stepRow ['B'] _ hi = hi
stepRow ('F':cs) low hi = stepRow cs low (((hi-low) `div` 2) + low)
stepRow ('B':cs) low hi = stepRow cs (((hi-low) `div` 2) + 1 + low) hi

stepCol :: [Char] -> Int -> Int -> Int
stepCol ['L'] low _ = low
stepCol ['R'] _ hi = hi
stepCol ('L':cs) low hi = stepCol cs low (((hi-low) `div` 2) + low) 
stepCol ('R':cs) low hi = stepCol cs (((hi-low) `div` 2) + 1 + low) hi 
  
code :: Int -> Int -> Int
code r c = r * 8 + c

findSeat :: [Char] -> Int
findSeat cs = code (stepRow (take 7 cs) 0 127) (stepCol (drop 7 cs) 0 7)

solve5 :: String -> IO ()
solve5 fn = do
  raw <- readFile fn
  print $ maximum $ map findSeat $ lines raw

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
  let codes = Set.fromList $ map findSeat $ lines raw 
  let codes' = foldr Set.delete codes $ [code 0 c | c <- [0..7]] ++ [code 127 c | c <- [0..7]] 
  let ordered = Set.toAscList codes' in
    print $ findGap ordered
    
