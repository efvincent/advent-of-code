module Y20.Day20 (solve20d20p1) where

import Data.List.Split (splitOn)
import qualified Data.IntMap as IM
import Data.List.Extra (intersect, intercalate)
import Linear.V2 ( V2(..) )
import Linear.V4 ( V4(..) )

type Point = V2 IM.Key
type Row = IM.IntMap Char
type Grid = IM.IntMap Row
type Blocks = IM.IntMap Block
data Dir = N | E | S | W deriving Show

data Block = Block
  { _num :: Int
  , _grid :: Grid
  , _dir :: Dir
  , _flip :: Bool }
  deriving (Show)

fn1 :: [Char]
fn1 = "./Data/20/Day20.ex01.txt"
fn :: [Char]
fn = "./Data/20/Day20.txt"

blockToStr :: Block -> String
blockToStr b =  intercalate "\n" $ map (map snd . IM.toList . (_grid b IM.!)) [0..9] 
    
bitListToInt :: [(Int, Char)] -> Int
bitListToInt = sum . map (\(n,c) -> if c == '#' then 2^n else 0)

blockSigs :: Block -> [Int]
blockSigs b =
  [s0, s1, s2, s3, s0', s1', s2', s3']
  where
    g = _grid b
    s0 = bitListToInt . IM.toList . (IM.! 0) $ g
    s1 = bitListToInt $ zip [0..] $ [getP b (V2 9 x) | x <- [0..9]]
    s2 = bitListToInt . IM.toList . (IM.! 9) $ g
    s3 = bitListToInt $ zip [0..] $ [getP b (V2 0 x) | x <- [0..9]]

    s0' = bitListToInt . zip [9,8..]. map snd . IM.toList . (IM.! 0) $ g
    s1' = bitListToInt . zip [9,8..] $ [getP b (V2 9 x) | x <- [0..9]]
    s2' = bitListToInt . zip [9,8..]. map snd . IM.toList . (IM.! 9) $ g
    s3' = bitListToInt . zip [9,8..] $ [getP b (V2 0 x) | x <- [0..9]]    

blockMatch :: Block -> Block -> Bool
blockMatch b1 b2 =
  (not . null) (blockSigs b1 `intersect` blockSigs b2)
  where
    bs1 = blockSigs b1
    bs2 = blockSigs b2

blockConnect :: Block -> Block -> Bool
blockConnect b1 b2 =
  (not . null) (blockSigs b1 `intersect` blockSigs b2)
  where
    bs1 = blockSigs b1
    bs2 = blockSigs b2


matchCount :: Blocks -> Int -> Int
matchCount bm bnum = 
  length . filter id . map (blockMatch (bm IM.! bnum) . snd) . IM.toList . IM.delete bnum $ bm 

parseBlock :: String -> Block
parseBlock raw =
  Block { _num = num
        , _grid = IM.fromList . zip [0..] . map mkRow $ ss
        , _dir = N
        , _flip = False }
  where
    (s:ss) = lines raw
    num = read . take 4 . drop 5 $ s
    mkRow s =  IM.fromList $ zip [0..] s

parseRaw :: String -> Blocks
parseRaw raw =
  IM.fromList . map (\s -> let b = parseBlock s in (_num b, b)) 
  $ splitOn "\n\n" raw

getP :: Block -> Point -> Char
getP b (V2 x y) = (IM.! y) (_grid b) IM.! x 

solve20d20p1 :: FilePath -> IO ()
solve20d20p1 fn = do
  raw <- readFile fn
  let bm = parseRaw raw in
    print $ product $ map fst $ filter (\(_,c) -> c == 2) $ map (\n -> (n,matchCount bm n)) $ IM.keys bm