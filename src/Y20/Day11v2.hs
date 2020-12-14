module Y20.Day11v2 (load1p, load2p, fn20d11) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Util (countTrue, stabilize)
import Data.Maybe (mapMaybe)
import Linear (V2(..))
type Point = V2 Int

data World = World
  { _neighbors :: M.Map Point (S.Set Point)
  , _state :: M.Map Point Bool }
  deriving (Show,Eq)

deltas :: [V2 Int]
deltas = [V2 dx dy |
    dx <- [- 1 .. 1], 
    dy <- [- 1 .. 1],
    not (dx == 0  && dy == 0)]

bbox :: [Point] -> ((Int, Int), (Int, Int))
bbox pts =
  ((minimum xs,minimum ys), (maximum xs, maximum ys))
  where
    xs = map (\(V2 x _) -> x) pts
    ys = map (\(V2 _ y) -> y) pts

mkRow :: (Int, String) -> [(Point,Bool)]
mkRow (y, row) = 
  let row' = zip [0..] row in
  map (\(p,v) -> (p, v == '#'))
  $ filter (\(_, v) -> v /= '.') 
  $ map (\(x, v) -> (V2 x y, v)) row'

nbors1 :: Point -> [Point]
nbors1 p = map (p+) deltas

nbors2 :: ((Int,Int),(Int,Int)) -> M.Map Point Bool -> Point -> [Point]
nbors2 ((minx,miny),(maxx,maxy))  st p =
  mapMaybe forDelta deltas
  where
    forDelta d =
      walk (p+d)
      where
        oob (V2 x y) = not (x >=minx && x <=maxx && y >=miny && y <=maxy)
        walk p | oob p = Nothing
        walk p | M.member p st = Just p 
        walk p = walk (p+d)

mkWorld1 :: String -> World
mkWorld1 s =
  World 
    { _neighbors = 
      M.fromList 
      $ map 
        (\(point,_) -> (point, S.intersection points $ S.fromList $ nbors1 point)) 
        pointStates
    , _state = M.fromList pointStates }
  where
    pointStates = concatMap mkRow 
      $ zip [0..] 
      $ lines s
    points = S.fromList $ map fst pointStates

mkWorld2 :: String -> World
mkWorld2 s =
  World 
    { _neighbors = 
      M.fromList 
      $ map 
        (\(point,_) -> (point, S.intersection pointSet $ S.fromList $ nbors2 bb pointMap point)) 
        pointStates
    , _state = pointMap }
  where
    pointStates = concatMap mkRow 
      $ zip [0..] 
      $ lines s
    pointSet = S.fromList $ map fst pointStates
    pointMap = M.fromList pointStates
    bb = bbox $ S.toList pointSet


-- | Given a seat threshold, a map of neighbors for each seat, and
-- the current state for all the seats, determine the new state for
-- each seat (if it changes) and return the map of states for all sets
step 
  :: Int                        -- ^ Exit seat threshold
  -> M.Map Point (S.Set Point)  -- ^ neighbors for each point
  -> M.Map Point Bool           -- ^ current state of the seats
  -> M.Map Point Bool           -- ^ returns new state of the world
step thresh neighbors curState =
  M.intersectionWith calcNewState neighbors curState
  where
    calcNewState nbrs curOccupied 
      | not curOccupied = not (any (curState M.!) nbrs)
      | curOccupied     =  
          let occNeighbors = countTrue (curState M.!) nbrs in
          occNeighbors < thresh

run1 World{_state = st, _neighbors = ns} =
  countTrue id $ map snd $ M.toList $ stabilize (step 4 ns) st

run2 World{_state = st, _neighbors = ns} =
  countTrue id $ map snd $ M.toList $ stabilize (step 5 ns) st

fn20d11 = "./data/20/Day11.txt"

fn20d11ex01 = "./data/20/Day11.ex01.txt"

load1 :: IO World
load1 = do
  raw <- readFile fn20d11ex01
  pure $ mkWorld1 raw

load1p :: IO World
load1p = do
  raw <- readFile fn20d11
  pure $ mkWorld1 raw  

load2 :: IO World
load2 = do
  raw <- readFile fn20d11ex01
  pure $ mkWorld2 raw

load2p :: IO World
load2p = do
  raw <- readFile fn20d11
  pure $ mkWorld2 raw  