module Y20.Day15 (solve20d15p2, solve20d15p1) where

import Data.List.Split (splitOn)
import qualified Data.IntMap as M
import Debug.Trace

t :: [Char]
t = "13,0,10,12,1,5,8"

t1 = "0,3,6"

type Data = M.IntMap (Int,Int)

parse :: String -> Data
parse s = 
  let xs = map read . splitOn "," $ s in
  M.fromList $ zip xs [(n,0)|n <- [1..]]

step :: Int -> Bool -> Int -> Int -> Data -> Int
step term lastWasFirst turn _ m
  | lastWasFirst = 
    -- last word was spoken for the first time
    if turn == term then speak 
    else step term False (turn+1) speak m'
    where
      speak = 0 
      (l1, _) = m M.! 0
      out = "turn:"  ++ show (turn `div` 1000000) ++ "MM\tlastWasFirst: " ++ show lastWasFirst ++ "\tthisWasFirst: False"  
        ++ "\tspeaking: " ++ show speak ++ "\tUpdating to " ++ show (turn, l1)      
      m' = M.insert speak (turn, l1) (if turn `mod` 1000000 == 0 then trace out m else m)

step term lastWasFirst turn l m 
  | not lastWasFirst = 
    -- word has been spoken
    if turn == term then speak 
    else step term thisWasFirst (turn+1) speak m'    
    where
      (l1,l2) = m M.! l
      speak = l1 - l2
      (thisWasFirst,spLast) = if M.member speak m then (False,fst $ m M.! speak) else (True,0)
      out = "turn:"  ++ show (turn `div` 1000000) ++ "MM\tlastWasFirst: " ++ show lastWasFirst ++ "\tthisWasFirst: " ++ show thisWasFirst 
        ++ "\tspeaking: " ++ show speak ++ "\tUpdating to " ++ show (turn,spLast)
      m' = M.insert speak (turn,spLast) (if turn `mod` 1000000 == 0 then trace out m else m)

solve20d15p1 :: IO ()
solve20d15p1 = do
  let ns = parse t1
  let ans = step 2020 True (length ns + 1) 6 ns
  print ans

solve20d15p2 :: IO ()
solve20d15p2 = do
  let ns = parse t
  let ans = step 30000000 True (length ns + 1) 8 ns
  print ans