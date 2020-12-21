module Y20.Day20 where

import Data.List.Split (splitOn)
import qualified Data.IntMap as IM
import Data.List.Extra (intercalate)
import Linear.V2 ( V2(..) )
import Linear.V4 ( V4(..) )

type Point = V2 IM.Key
type Block = IM.IntMap (IM.IntMap Char)

fn1 :: [Char]
fn1 = "./Data/20/Day20.ex01.txt"
fn :: [Char]
fn = "./Data/20/Day20.txt"

blockToStr :: Block -> String
blockToStr b =  intercalate "\n" $ map (map snd . IM.toList . (b IM.!)) [0..9] 
    
bitListToInt :: [(Int, Char)] -> Int
bitListToInt = sum . map (\(n,c) -> if c == '#' then 2^n else 0)

blockSigs :: Block -> V4 Int
blockSigs b =
  V4 s0 s1 s2 s3
  where
    s0 = bitListToInt . IM.toList . (IM.! 0) $ b
    s1 = bitListToInt $ zip [0..] $ [getP b (V2 9 x) | x <- [0..9]]
    s2 = bitListToInt . IM.toList . (IM.! 9) $ b
    s3 = bitListToInt $ zip [0..] $ [getP b (V2 0 x) | x <- [0..9]]

parseBlock :: String -> (Int, Block)
parseBlock raw =
  (num, IM.fromList . zip [0..] . map mkRow $ ss)
  where
    (s:ss) = lines raw
    num = read . take 4 . drop 5 $ s
    mkRow s =  IM.fromList $ zip [0..] s

parseRaw :: String -> IM.IntMap Block
parseRaw raw =
  IM.fromList . map parseBlock $ blocks
  where
    blocks = splitOn "\n\n" raw

getP :: Block -> Point -> Char
getP b (V2 x y) = (IM.! y) b IM.! x 