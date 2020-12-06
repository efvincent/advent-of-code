module Y18.Day01 where

import qualified Data.Set as Set

apply :: Int -> String -> Int
apply acc ('+':rest)= read rest + acc
apply acc ('-':rest)= acc - read rest

applyAndTrack :: Set.Set Int -> Int -> [String] -> [String] -> Int
applyAndTrack s acc [] origOps = applyAndTrack s acc origOps origOps
applyAndTrack s acc _ _ | Set.member acc s = acc
applyAndTrack s acc (op:ops) origOps =
  let acc' = apply acc op in
  applyAndTrack (Set.insert acc s) acc' ops origOps

calc1 :: String -> Int
calc1 raw = foldl apply 0 (lines raw)

solve1 :: IO ()
solve1 = do
  raw <- readFile "./data/18/Day01.txt"
  print $ show $ foldl apply 0 (lines raw)

solve2 :: IO ()
solve2 = do
  raw <- readFile "./data/18/Day01.txt"
  let origOps = lines raw
  print $ applyAndTrack Set.empty 0 origOps origOps