module Y20.Day05 where
import qualified Data.Set as Set

filename :: [Char]
filename = "./data/20/Day05.txt"

{-
whoa ... check this out:
  BFFFBFBLLR -> 553
  1000101001 -> 553
this means these seat codes are just binary where
  B/R => 1 and F/L => 0
  could have written findSeat and Step to take advantage :/
  lesson for next time. 
-}

-- | c1 and c2 are the encoding chars, either 'F' 'B' or 'L' 'R'. This function 
-- pattern matches the `[Char]` list. When there's only one char left `[c]` the
-- guard chooses either the low or high remaining value. Otherwise, it either 
-- picks the top or bottom half and recurses
step :: Char -> Char -> [Char] -> Int -> Int -> Int
step c1 _  [c] low _ | c == c1 = low
step _  c2 [c] _ hi  | c == c2 = hi
step c1 c2 (c:cs) low hi | c == c1 = step c1 c2 cs low (((hi-low) `div` 2) + low)
step c1 c2 (c:cs) low hi | c == c2 = step c1 c2 cs (((hi-low) `div` 2) + 1 + low) hi

code :: Int -> Int -> Int
code r c = r * 8 + c

findSeat :: [Char] -> Int
findSeat cs = code (step 'F' 'B' (take 7 cs) 0 127) (step 'L' 'R' (drop 7 cs) 0 7)

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
  let ordered = 
        Set.toAscList
        $ foldr Set.delete (Set.fromList $ map findSeat $ lines raw)
        $ [code 0 c | c <- [0..7]] ++ [code 127 c | c <- [0..7]] in
    print $ findGap ordered
