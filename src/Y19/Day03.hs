module Y19.Day03 (solve19day03p1) where

import Util (fileOfLinesOfCSVStringsToLists)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
type Instruction = (Char, Int)
type Pos = (Int,Int,Int)
type Segment = (Pos,Pos)
type Wire = Set.Set Pos

-- | Used in the calculation of intersecting lines. Can be made simpler if we use
-- a bounding box style approach
data Orientation = CO | CW | CCW
instance Eq Orientation where
  (==) CO CO = True
  (==) CW CW = True
  (==) CCW CCW = True
  (==) _ _ = False

test = do
  lol <- fileOfLinesOfCSVStringsToLists "./data/Day03.txt"
  print lol

steps :: Pos -> Instruction -> [Pos]
steps (x,y,l) (dir, amp) =
  case dir of
    'U' -> [(x,y',l) | y' <- [(y - amp)..y]]
    'D' -> [(x,y',l) | y' <- [(y + amp),(y + amp -1)..y]]
    'R' -> [(x',y,l) | x' <- [(x + amp),(x + amp -1)..x]]
    'L' -> [(x',y,l) | x' <- [(x - amp)..x]]
    _   -> []

segment :: Pos -> Instruction -> Pos
segment (x,y,l) (dir, amp) =
  case dir of
    'U' -> (x,       y - amp, l + amp)
    'D' -> (x,       y + amp, l + amp)
    'R' -> (x + amp, y      , l + amp)
    'L' -> (x - amp, y      , l + amp)
    _   -> (x,y,l)

toIns :: String -> Instruction
toIns (s:ss) = (s, read ss :: Int)
toIns [] = error "Raw instruction is an empty string"

buildSegments :: [Segment] -> [Instruction] -> [Segment]
buildSegments segments [] = segments
buildSegments segments (instr:instrs) = 
  buildSegments segments' instrs
  where
    curEnd = if null segments then (0,0,0) else snd $ head segments
    newEnd = segment curEnd instr
    segments' = (curEnd, newEnd) : segments

orientation :: Pos -> Pos -> Pos -> Orientation
orientation (px,py,_) (qx,qy,_) (rx,ry,_) =
  case (qy - py) * (rx - qx) - (qx - px) * (ry - qy) of
    0         -> CO
    v | v > 0 -> CW
    _         -> CCW

onSeg :: Pos -> Pos -> Pos -> Bool
onSeg (px,py,_) (qx,qy,_) (rx,ry,_) =
  qx <= max px rx && 
  qx >= max px rx &&
  qy <= max py ry &&
  qy >= max py ry

-- | Simple numeric range generator that goes up or down by step 1
-- depending on if a > b or b > a
rng :: (Eq a, Num a, Enum a) => a -> a -> [a]
rng a b | a == b = [a]
rng a b = [a, a + signum (b - a)..b]

pointsOnSeg :: Segment -> [Pos]
pointsOnSeg ((x1,y1,l1),(x2,y2,_)) =
  let ps = [(x,y) | x <- rng x1 x2, y <- rng y1 y2] in
  map (\(x,y) -> (x,y,abs (abs x-x1 + abs y-y1) + l1)) ps

-- | True if the two segments intersect, otherwise false
intersects :: Segment -> Segment -> [Pos]
intersects (p1,q1) (p2,q2) =
  let o1 = orientation p1 q1 p2 in 
  let o2 = orientation p1 q1 q2 in
  let o3 = orientation p2 q2 p1 in 
  let o4 = orientation p2 q2 q1 in
    if (o1 /= o2 && o3 /= o4 ) 
    || o1 == CO && onSeg p1 p2 q1  
    || o2 == CO && onSeg p1 q2 p1  
    || o3 == CO && onSeg p2 p1 q2  
    || o4 == CO && onSeg p2 q1 q2 
    then inter (p1,q1) (p2, q2) 
    else []
  where
    -- inter :: Segment -> Segment -> [Pos]
    -- inter s1 s2 = 
    --   let pointsOn1 = Set.fromList $ pointsOnSeg s1 in        
    --   [p | p <- pointsOnSeg s2, Set.member p pointsOn1]

    inter :: Segment -> Segment -> [Pos]
    inter s1 s2 =
      let pointsOn1 = Map.fromList $ map (\(x,y,l) -> ((x,y), (x,y,l))) (pointsOnSeg s2)  in
      foldl 
        (\acc (x,y,l) ->
          -- TODO: WIP!
          case (trace (show pointsOn1) pointsOn1) Map.!? (x,y) of
            Just (_,_,l2) -> (x,y,l+l2):acc
            Nothing -> acc
        ) [] s1

closestIntersect :: [String] -> [String] -> Maybe Int
closestIntersect raw1 raw2 =
  let segs1 = buildSegments [] $ map toIns raw1 in
  let segs2 = buildSegments [] $ map toIns raw2 in
  let allInters = concat [intersects s1 s2 | s1 <- segs1, s2 <- segs2] in
    case filter (/= 0) $ map (\(x,y,_) -> abs x + abs y) allInters of
      [] -> Nothing
      dists -> Just $ minimum dists

-- cheapestIntersect :: [String] -> [String] -> Maybe Int
-- cheapestIntersect raw1 raw2 =
--   let segs1 = buildSegments [] $ map toIns raw1 in
--   let segs2 = buildSegments [] $ map toIns raw2 in  
--   let allInters = concat [intersects s1 s2 | s1 <- segs1, s2 <- segs2] in

solve19day03p1 :: FilePath -> IO (Maybe Int)
solve19day03p1 filename = do
  raw <- fileOfLinesOfCSVStringsToLists filename
  return $
    case raw of
      [] -> Nothing
      (raw1:raw2:[]) -> closestIntersect raw1 raw2
      _ -> Nothing