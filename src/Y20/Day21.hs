module Y20.Day21 (solve20d21p1, solve20d21p2) where

import qualified Data.Set as S
import Data.Set (Set) 
import Data.Map (Map)
import qualified Data.Map as M

import Data.List.Split (splitOn)
import Data.Functor ((<&>))
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.List (intercalate)
import Util (pickUnique, countTrue)

t1 :: [Char]
t1 = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)"

fn = "./Data/20/Day21.txt"

parseRaw :: String -> [(Set [Char], Set [Char])]
parseRaw = 
  map (((\ [s1, s2] -> (S.fromList . splitOn " " $ s1, S.fromList . splitOn ", " $ s2))
      . splitOn " (contains ") . init) . lines

assembleOptions
    :: (Ord k, Ord a)
    => [(Set a, Set k)]
    -> [(k, Set a)]
assembleOptions info = M.toList . M.unionsWith S.intersection $
    info <&> \(igr, alg) -> M.fromSet (const igr) alg

solve1 :: (Ord a, Ord k) => [(Set a, Set k)] -> Maybe Int
solve1 d = 
  fmap (countNotIn (concatMap (toList . fst) d))
    . listToMaybe
    . map (S.fromList . toList)
    . pickUnique
    . assembleOptions $ d
    where
    countNotIn xs bad = countTrue (`S.notMember` bad) xs

solve2 :: (Ord k, Ord a) => [(Set a, Set k)] -> Maybe [a]
solve2 =
  fmap toList . listToMaybe . pickUnique . assembleOptions 

solve20d21p1 :: IO ()
solve20d21p1 = do
  raw <- readFile fn
  print $ solve1 . parseRaw $ raw    

solve20d21p2 :: IO ()
solve20d21p2 = do
  raw <- readFile fn
  print $ fmap (intercalate "," . toList) . listToMaybe . pickUnique . assembleOptions . parseRaw $ raw