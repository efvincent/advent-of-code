module Y20.Day23v2 (data20d32, solve20d23p2) where

import qualified Data.IntMap.Strict as IM
import Data.IntMap ((!))
import Util (applyN)
import Data.Char (digitToInt)

t1 :: [Char]
t1 = "389125467"

data20d32 :: [Char]
data20d32 = "394618527" -- bluecore account puzzle: "583976241"

turn :: Int -> (Int, IM.IntMap Int) -> (IM.Key, IM.IntMap IM.Key)
turn maxx (curIdx, m) = 
  go $ dec' $ dec curIdx
  where
    hand1 = m ! curIdx
    hand2 = m ! hand1
    hand3 = m ! hand2
    next  = m ! hand3
    dec' x = if x == hand1 || x == hand2 || x == hand3 then dec' $ dec x else x
    go x = 
      (next, IM.insert hand3 (m ! x) $ IM.insert x hand1 $ IM.insert curIdx next m)
    dec :: Int -> Int
    dec 0 = maxx - 1
    dec x = x - 1
    
parseRaw :: Int -> String -> (Int, IM.IntMap Int)
parseRaw padToN s =
  (head xs', xs'') 
  where
    xs = map digitToInt s 
    xs' = map (+ (-1)) xs ++ [length xs .. padToN - 1]
    xs'' = IM.fromList $ zip xs' (tail $ cycle xs')

play :: Int -> Int -> (Int, IM.IntMap Int) -> Int
play maxx nIters (first, xmap) =
  v1 * v2
  where
    (v1:v2:_) = map (+1) $ mapToList 0 0 $ snd $ applyN nIters (turn maxx) (first, xmap)
    mapToList stopIdx idx m = 
      let cur = m ! idx in
        if cur == stopIdx then [] else cur : mapToList stopIdx cur m

-- | Solve Day 23. This function needs to run compiled; interpreted in ghci
-- won't work because of the resource demands and necessary optimizations that
-- come in the compiled version.
--
-- For the puzzle use these values:
--    @padToN@ = 1000000 
--    @turns@  = 10000000

solve20d23p2 
  :: Int      -- ^| pad out to this number
  -> Int      -- ^| number of turns to play
  -> String   -- ^| raw input
  -> IO ()
solve20d23p2 padToN turns raw = 
  let cups = parseRaw padToN raw in
  print $ play padToN turns cups
