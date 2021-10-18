module Y20.Day17p2 (solve20d17p2) where

import qualified Data.Set as S
import Linear (V2(..), V4(..))

type Point = V4 Int
type Cube =  S.Set Point 

t1 :: [Char]
t1 = ".#.\n..#\n###"
t2 :: [Char]
-- bluecore account puzzle was: t2 = ".###.#.#\n####.#.#\n#.....#.\n####....\n#...##.#\n########\n..#####.\n######.#"
t2 = "######.#\n#.###.#.\n###.....\n#.####..\n##.#.###\n.######.\n###.####\n######.#"

initCube :: String -> S.Set Point 
initCube s =
  S.fromList        
      $ map fst
      $ filter snd
      $ concatMap
        (\ (y, xs) -> map (\ (x, c) -> (V4 x y 0 0, c == '#')) xs)
        (zip [0 .. ] $ map (zip [0 .. ]) $ lines s)

bounds :: Cube -> V4 (V2 Int)
bounds c =
  V4 xb yb zb wb
  where
    vs = S.toList c
    xb = let xs = map (\(V4 x _ _ _) -> x) vs in V2 (minimum xs) (maximum xs)
    yb = let ys = map (\(V4 _ y _ _) -> y) vs in V2 (minimum ys) (maximum ys)
    zb = let zs = map (\(V4 _ _ z _) -> z) vs in V2 (minimum zs) (maximum zs)
    wb = let ws = map (\(V4 _ _ _ w) -> w) vs in V2 (minimum ws) (maximum ws)

neighbors :: Point -> [Point]
neighbors (V4 x y z w) =
  [V4 x' y' z' w' | 
    x' <- [x-1 .. x+1],  
    y' <- [y-1 .. y+1], 
    z' <- [z-1 .. z+1],
    w' <- [w-1 .. w+1], (x',y',z',w') /= (x,y,z,w)]

expand :: V4 (V2 Int) -> V4 (V2 Int)
expand (V4 (V2 x1 x2) (V2 y1 y2) (V2 z1 z2) (V2 w1 w2)) = 
  V4 (V2 (x1-1) (x2+1)) (V2 (y1-1) (y2+1)) (V2 (z1-1) (z2+1)) (V2 (w1-1) (w2+1))

check :: Cube -> Point -> Bool
check c p =
  let l = length . filter id . map (`S.member` c) . neighbors $ p in
  if S.member p c then l == 2 || l == 3 else l == 3

step :: Cube -> Cube
step c = 
  foldl (\acc p -> if check c p then S.insert p acc else S.delete p acc) S.empty todo
  where 
    (V4 (V2 x1 x2) (V2 y1 y2) (V2 z1 z2) (V2 w1 w2)) = expand $ bounds c
    todo = [V4 x y z w | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2], w <- [w1..w2]]

run :: (Num a, Enum a) => a -> Cube -> Int
run n c = length $ foldl (\acc _ -> step acc) c [1..n]

solve20d17p2 :: IO ()
solve20d17p2 = print $ run 6 $ initCube t2
