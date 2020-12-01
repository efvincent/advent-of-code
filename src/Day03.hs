{-# LANGUAGE TypeSynonymInstances #-}

module Day03 where

import Util (fileOfLinesOfCSVStringsToLists)
import qualified Data.Set as Set

type Instruction = (Char, Int)
type Pos = (Int,Int)
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
steps (x,y) (dir, amp) =
  case dir of
    'U' -> [(x,y') | y' <- [(y - amp)..y]]
    'D' -> [(x,y') | y' <- [(y + amp),(y + amp -1)..y]]
    'R' -> [(x',y) | x' <- [(x + amp),(x + amp -1)..x]]
    'L' -> [(x',y) | x' <- [(x - amp)..x]]
    _   -> []

segment :: Pos -> Instruction -> Pos
segment (x,y) (dir, amp) =
  case dir of
    'U' -> (x,       y - amp)
    'D' -> (x,       y + amp)
    'R' -> (x + amp, y)
    'L' -> (x - amp, y)
    _   -> (x,y)

toIns :: String -> Instruction
toIns (s:ss) = (s, read ss :: Int)
toIns [] = error "Raw instruction is an empty string"

buildSegments :: [Segment] -> [Instruction] -> [Segment]
buildSegments segments [] = segments
buildSegments segments (instr:instrs) = 
  buildSegments segments' instrs
  where
    curEnd = if null segments then (0,0) else snd $ head segments
    newEnd = segment curEnd instr
    segments' = (curEnd, newEnd) : segments

orientation :: Pos -> Pos -> Pos -> Orientation
orientation (px,py) (qx,qy) (rx,ry) =
  case (qy - py) * (rx - qx) - (qx - px) * (ry - qy) of
    0         -> CO
    v | v > 0 -> CW
    _         -> CCW

onSeg :: Pos -> Pos -> Pos -> Bool
onSeg (px,py) (qx,qy) (rx,ry) =
  qx <= max px rx && 
  qx >= max px rx &&
  qy <= max py ry &&
  qy >= max py ry

pointsOnSeg :: Segment -> [Pos]
pointsOnSeg ((x1,y1),(x2,y2)) =
  let (x1',x2') = if x2 < x1 then (x2,x1) else (x1,x2) in
  let (y1',y2') = if y2 < y1 then (y2,y1) else (y1,y2) in
  [(x,y) | x <- [x1'..x2'], y <- [y1'..y2']]

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
    inter :: Segment -> Segment -> [Pos]
    inter s1 s2 = 
      let pointsOn1 = Set.fromList $ pointsOnSeg s1 in        
      [p | p <- pointsOnSeg s2, Set.member p pointsOn1]

closestIntersect :: [String] -> [String] -> Maybe Int
closestIntersect raw1 raw2 =
  let segs1 = buildSegments [] $ map toIns raw1 in
  let segs2 = buildSegments [] $ map toIns raw2 in
  let allInters = concat [intersects s1 s2 | s1 <- segs1, s2 <- segs2] in
    case filter (/= 0) $ map (\(x,y) -> abs x + abs y) allInters of
      [] -> Nothing
      dists -> Just $ minimum dists


solvePart1 :: FilePath -> IO (Maybe Int)
solvePart1 filename = do
  raw <- fileOfLinesOfCSVStringsToLists filename
  return $
    case raw of
      [] -> Nothing
      (raw1:raw2:[]) -> closestIntersect raw1 raw2
      _ -> Nothing