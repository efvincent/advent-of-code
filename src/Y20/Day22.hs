module Y20.Day22 (solve20d22p2,solve20d22p1)  where

import Data.List.Split (splitOn)
import Debug.Trace (trace)
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Sequence (Seq(..), (|>), fromList)
import qualified Data.Sequence as SEQ
import Data.Foldable (toList)
import Data.Hashable (hash)

type Deck = Seq Int

fn :: [Char]
fn = "./Data/20/Day22.txt"

parseRaw :: [Char] -> [(Integer, Deck)]
parseRaw = 
  zip [1..] . map (fromList . map read . tail . lines) . splitOn "\n\n" 
  
score :: Deck -> Int
score xs = sum . zipWith (*) [length xs, length xs-1 .. 1] $ toList xs

play1 ::[(b, Deck)] -> (Int, String, Int)
play1 g =
  let [p1,p2] = map snd g in go 1 p1 p2
  where
    go :: Int -> Deck -> Deck -> (Int, String, Int)
    go c (x :<| xs) (y :<| ys)
      | x > y = go (c+1) (xs |> x |> y) ys
      | y > x = go (c+1) xs (ys |> y |> x)
    go c p1 _ | not . null $ p1 = (c, "player 1", score p1)
    go c _ p2 | not . null $ p2 = (c, "player 2", score p2)

mkHash :: Deck -> Deck -> Int
mkHash xs ys = hash (take 2 (toList xs), take 2 (toList ys), length xs)

play2 :: [(a, Deck)] -> (Int, Int)
play2 g =
  let [p1,p2] = map snd g in 
  let (winner, hand) = go IS.empty p1 p2 in
  (winner, score hand) 
  where
    go :: IntSet -> Deck -> Deck -> (Int, Deck)
    go prevHands p1@(x :<| xs) p2@(y :<| ys)
      | IS.member (mkHash p1 p2) prevHands = (1, p1)
      | length xs >= x && length ys >= y = 
        let prevHands' = IS.insert (mkHash p1 p2) prevHands in
        let (winner, wh) = go prevHands' (SEQ.take x xs) (SEQ.take y ys) in
        if winner == 1 
            then go prevHands' (xs :|> x :|> y) ys
            else go prevHands' xs (ys :|> y :|> x) 
      | otherwise =
        let prevHands' = IS.insert (mkHash p1 p2) prevHands in
        if x > y 
          then go prevHands' (xs :|> x :|> y) ys
          else go prevHands' xs (ys :|> y :|> x)
    go _ p1 _ | not . null $ p1 = (1, p1)
    go _ _ p2 | not . null $ p2 = (2, p2)

solve20d22p1 :: IO ()
solve20d22p1 = do
  raw <- readFile fn
  print $ play1 $ parseRaw raw

solve20d22p2 :: IO ()
solve20d22p2 = do
  raw <- readFile fn
  print $ play2 $ parseRaw raw