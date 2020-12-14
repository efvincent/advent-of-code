module Y20.Day14 where

import qualified Data.IntMap as M
-- import Data.List (isPrefixOf)
import Text.Regex.TDFA

data State = State
  { _mask :: [(M.Key, Int)] 
  , _mem :: M.IntMap Int }
  deriving (Show)

data Op = Mask [(M.Key, Int)]
        | Write Int (M.IntMap Int)
        deriving (Show)

initState = State { _mask = [], _mem = M.empty }

toBin :: Int -> M.IntMap Int
toBin n =
  let b = toBinary n
  in M.fromList $ zip [35,34..0] $ replicate (36 - length b) 0 ++ b
  where
    toBinary :: Int -> [ Int ]
    toBinary 0 = [ 0 ]
    toBinary n = toBinary ( n `quot` 2 ) ++ [ n `rem` 2 ]

fromBin :: M.IntMap Int -> Int
fromBin m =
  foldl (\acc (p,bit) -> acc + (bit * (2^p))) 0 l
  where
    l = M.toList m

toMask :: String -> [(Int,Int)]
toMask s =
  filter (\(_,b) -> b /= (-1)) 
    $ zip [35,34..0] 
    $ map (\c -> if c == 'X' then -1 else read [c]) s

applyMask :: M.IntMap a -> [(M.Key, a)] -> M.IntMap a
applyMask = foldl (\vals (k,v) -> M.insert k v vals)

parseWrite :: String -> Op
parseWrite s =
  Write a bin
  where
    [addr,v] = getAllTextMatches (s =~ "[0-9]+") :: [String]
    a = read addr :: Int
    b = read v :: Int
    bin = toBin b

parseLine :: String -> Op 
parseLine s 
  | s =~ "mask" = Mask $ toMask $ s =~ "[X01]{36}"
  | s =~ "mem"  = parseWrite s
  
runOp :: State -> Op -> State
runOp s (Mask m) = s { _mask = m }
runOp s@State{_mem=mem, _mask=mask} (Write addr num) = 
  s { _mem = M.insert addr (fromBin $ applyMask num mask) mem }

