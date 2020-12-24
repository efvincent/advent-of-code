module Y20.Day22 (solve20d22p1) where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

fn :: [Char]
fn = "./Data/20/Day22.txt"

parseRaw :: [Char] -> [(Integer, [String])]
parseRaw = zip [1..] . map (tail . lines) . splitOn "\n\n"

play :: Num a => [(b, [String])] -> (a, String, Int)
play g =
  let [p1,p2] = map (map read . snd) g in go 1 p1 p2
  where
    score xs = sum . zipWith (*) [length xs, length xs-1 .. 1] $ xs
    go c (x:xs) (y:ys)
      | x > y = go (c+1) (xs ++ [x,y]) ys
      | y > x = go (c+1) xs (ys ++ [y,x])
    go c p1@(x:xs) [] = (c, "player 1", score p1)
    go c [] p2@(y:ys) = (c, "player 2", score p2)

solve20d22p1 :: IO ()
solve20d22p1 = do
  raw <- readFile fn
  print $ play $ parseRaw raw