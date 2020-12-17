{-# LANGUAGE LambdaCase #-}
module Y20.Day14 (solve20d14p2, solve20d14p1) where

import qualified Data.IntMap as M
import Text.Regex.TDFA ( (=~), AllTextMatches(getAllTextMatches) )
import Util (genBitPermutations)
import Data.Char (digitToInt)

type Bin = M.IntMap Int
type Memory = M.IntMap Int
type Mask = [(M.Key, Int)]

data State = State
  { _masks :: [Mask] 
  , _mem :: Memory }
  deriving (Show)

data Op = Mask [Mask]
        | Write Int Bin
        deriving (Show)

initState :: State
initState = State { _masks = [], _mem = M.empty }

toBin ::  Int -> Bin
toBin n =
  let b = toBinary n
  in M.fromList $ zip [35,34..0] $ replicate (36 - length b) 0 ++ b
  where
    toBinary :: Int -> [ Int ]
    toBinary 0 = [ 0 ]
    toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

fromBin :: Bin -> Int
fromBin m =
  foldl (\acc (p,bit) -> acc + (bit * (2^p))) 0 l
  where
    l = M.toList m

-- | First interpretation of the mask. X is unused and ignored. 0 or 1 overwrite
-- the target value and are recorded as is
toMask1 :: String -> [Mask]
toMask1 s =
  [filter ((/= -1) . snd)  
    $ zip [35,34..0] 
    $ map (\c -> if c == 'X' then -1 else digitToInt c) s]

-- | Second interpretation of the mask. 0 is unused and ignored. X is meant to 
-- expand into both 1 and 0, therefore toMask2 expands to a list of masks to be
-- applied
toMask2 :: String -> [Mask]
toMask2 s =
  map M.toList masks
  where
  rawMask = 
    filter ((/= 0) . snd) 
    $ zip [35,34..0] 
    $ map (\case 
        'X' -> 2
        c -> digitToInt c) s
  template = M.fromList rawMask
  indexes = map fst $ filter ((==2) . snd) rawMask
  mods = map (zip indexes) $ genBitPermutations $ length indexes
  masks = foldl (\acc' mod ->  let mask = foldl (\acc (k,v) -> M.insert k v acc) template mod in mask:acc') [] mods

applyMask :: Bin -> Mask -> Bin
applyMask = foldl (\vals (k,v) -> M.insert k v vals)

parseWrite :: String -> Op
parseWrite s =
  Write a bin
  where
    [addr,v] = getAllTextMatches (s =~ "[0-9]+") :: [String]
    a = read addr :: Int
    b = read v :: Int
    bin = toBin b

parseLine :: (String -> [Mask]) -> String -> Op 
parseLine toMaskFn s 
  | s =~ "mask" = Mask $ toMaskFn $ s =~ "[X01]{36}"
  | s =~ "mem"  = parseWrite s
  
runOp1 :: State -> Op -> State
runOp1 s (Mask m) = s { _masks = m }
runOp1 s@State{_mem=mem, _masks=mask} (Write addr num) = 
  s { _mem = M.insert addr (fromBin $ applyMask num $ head mask) mem }

runOp2 :: State -> Op -> State
runOp2 s (Mask m) = s { _masks = m }
runOp2 s@State{_mem=mem, _masks=masks } (Write addr num) =
  s { _mem = foldl doOne mem masks }
  where
    addrBin = toBin addr
    num' = fromBin num
    doOne :: Memory -> Mask -> Memory
    doOne mem' mask =
      let addr' = fromBin $ applyMask addrBin mask
      in M.insert addr' num' mem'

solve :: (String -> [Mask]) -> (State -> Op -> State) -> String -> Int
solve toMask runOp raw =
  let ops = map (parseLine toMask) $ lines raw
  in let State{_mem=mem} = foldl runOp initState ops
  in sum $ map snd $ M.toList mem

solve20d14p1 :: IO ()
solve20d14p1 = do
  raw <- readFile "./Data/20/Day14.txt"
  let ans = solve toMask1 runOp1 raw
  print ans
  
solve20d14p2 :: IO () -- (3.10 secs, 6,303,551,160 bytes)
solve20d14p2 = do
  raw <- readFile "./Data/20/Day14.txt"
  let ans = solve toMask2 runOp2 raw
  print ans
