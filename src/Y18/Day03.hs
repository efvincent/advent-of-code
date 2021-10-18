module Y18.Day03 where

import Linear.V2 (V2(..))
import Data.List.Extra (splitOn)
import Util (freqs, vsnd, vfst)
import Data.Ix (range)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Foldable (find)

type Point = V2 Int
type PointFreqs = Map Point Int
data Rect = Rect { _p1 :: Point, _p2 :: Point } deriving Show
data Patch = Patch 
  { _num :: Int
  , _rect :: Rect } deriving Show

{------------------------
  SOLUTIONS
-------------------------}

patchPoints :: Patch -> [Point]
patchPoints Patch{ _rect = Rect {_p1=p1, _p2=p2}} = 
  range (p1, p2)

pointFreqs :: [Patch] -> PointFreqs
pointFreqs = freqs . concatMap patchPoints

solve1 :: [Patch] -> Int
solve1 = length . filter (>=2) . M.elems . pointFreqs

noOverlap :: PointFreqs -> Patch -> Bool
noOverlap pmap r = 
  all (\p -> M.lookup p pmap == Just 1) (patchPoints r)

solve2 :: [Patch] -> Maybe Int
solve2 ps = fmap _num . find (noOverlap stakes) $ ps
  where
    stakes = pointFreqs ps
{------------------------
  PARSING
-------------------------}

parseRaw :: String -> [Patch]
parseRaw s =
  map parseLine (lines s)
  where
    parseLine ln =
      let [l,r] = splitOn ":" ln in
      let [sNum, sPoint] = splitOn "@" l in
      let [sx,sy] = splitOn "," sPoint in
      let num = read (tail sNum) in
      let p = V2 (read sx)  (read sy) in
      let [sw,sh] = splitOn "x" r in
      let wh = V2 (read sw - 1) (read sh - 1) in
      Patch 
        { _num = num
        , _rect = Rect  { _p1 = p
                        , _p2 = p + wh } }

{------------------------
  FILE NAMES
-------------------------}
fn :: String
fn = "./data/18/Day03.txt"