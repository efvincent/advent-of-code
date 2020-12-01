module Day03 where

import Util (fileOfLinesOfCSVStringsToLists)
import qualified Data.Set as Set

type Instruction = (Char, Int)
type Pos = (Int,Int)
type Segment = (Pos,Pos)
type Wire = Set.Set Pos

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

buildPath :: Wire -> Pos -> [Instruction] -> (Pos,Wire)
buildPath wire curPos [] = (curPos,wire)
buildPath wire curPos (instr:instrs) =
  buildPath wire' newCurPos instrs
  where
    -- generate new positions, get the new current position (head of new positions)
    -- and fold the new positions into the set that is the wire
    newPoss = steps curPos instr
    newCurPos = head newPoss
    wire' = foldl (flip Set.insert) wire newPoss

solvePart1 :: ([String],[String]) -> Int
solvePart1 (wire1, wire2) =
  0   

