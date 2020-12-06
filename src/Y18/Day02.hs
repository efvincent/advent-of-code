module Y18.Day02 where

import Util
import qualified Data.Set as Set

procLine :: [Char] -> Set.Set Int
procLine = Set.fromList . filter (>1) . map fst . histogram 

solve1 :: [Char] -> Int
solve1 raw =
  (numOf [2] + twosAndThrees) * (numOf [3] + twosAndThrees)
  where
    counts = map (Set.toList . procLine) $ lines raw 
    numOf x = length $ filter (== x) counts
    twosAndThrees = numOf [2,3]

