module Y20.Day09 (solve20d9p1, solve20d9p2, fn20d9) where

import           Data.Map.Strict (Map, fromList, (!))

type Preable = Int
type PreCalcMap = Map (Int,Int) Int

fn20d9e1 :: String
fn20d9e1 = "./data/20/Day09.ex01.txt"

fn20d9 :: String
fn20d9 = "./data/20/Day09.txt"

-- | Pre-calculates a map where the key is the index of two numbers that when added
-- equal the value.
mkPreCalcMap :: [Int] -> PreCalcMap
mkPreCalcMap ns =
  let idxNs = zip [0..] ns in
  fromList ([((na, nb), a + b) | (na,a) <- idxNs, (nb,b) <- idxNs])

-- | Checks if a number located at an index in the list can be found in set of candidates
checkNum :: Preable -> PreCalcMap -> (Int, Int) -> Bool
checkNum pre m (idx, n) =
  loop $ candidates pre m idx
  where
    loop :: [Int] -> Bool
    loop [] = False
    loop (c:_) | c == n = True
    loop (_:cs) = loop cs

    -- | The list of candidates is found by getting the cross product pairs within the range
    -- of `pre` indexes before the current index. Note the condition `, a < b` eliminates cases
    -- where `(a,b) == (b,a)` which we don't need to check b/c the sum would be the same
    candidates :: Preable -> PreCalcMap -> Int -> [Int]
    candidates pre m idx =
      if idx < pre then [] else
      map (m !) keys
      where
        i = idx - pre
        j = idx - 1
        keys = [(a,b) | a <- [i..j], b <- [i..j], a < b]

-- | works by walking the list checking each number against the previous `pre` numbers
-- in the list until it finds one that doesn't meet the criteria
findInvalid :: Preable -> [Int] -> Int
findInvalid pre ns =
  loop pre (drop pre ns)
  where
    m = mkPreCalcMap ns
    loop :: Int -> [Int] -> Int
    loop _ [] = 0
    loop idx (n:ns) | checkNum pre m (idx, n) = loop (idx+1) ns
    loop _ (n:_) = n

-- | works by accumulating a list until the sum of the list == the target number. The
-- accumulation starts at the head of the `ns` list which is initially the whole list.
-- When the sum > target, it starts building a new list, but this time from the tail of
-- the source list. In this way each trial starts a list one position further down the
-- list of all numbers
findSumSubListEqTarget :: Int -> [Int] -> [Int]
findSumSubListEqTarget target allNums =
  loop [] allNums (tail allNums)
  where
    loop :: [Int] -> [Int] -> [Int] -> [Int]
    loop _ _ [] = []
    loop acc (cur:rest) nextTrial =
      case sum $ cur:acc of
        test | test == target -> cur:acc
        test | test > target  -> loop [] nextTrial (tail nextTrial)
        _                     -> loop (cur:acc) rest nextTrial
    loop _ _ _ = []

solve20d9p1 :: Int -> String -> IO ()
solve20d9p1 pre s = do
  raw <- readFile s
  let ns = map read $ lines raw
  print $ findInvalid pre ns

solve20d9p2 :: Int -> String -> IO ()
solve20d9p2 pre s = do
  raw <- readFile s
  let ns = map read $ lines raw
  let inv = findInvalid pre ns
  let cs = findSumSubListEqTarget inv ns
  let ans = minimum cs + maximum cs
  putStrLn $ "invalid: " ++ show inv ++ "\ncalc set: " ++ show cs ++ "\nans: " ++ show ans
