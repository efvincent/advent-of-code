module Y20.Day17 (solve20d17p1) where

import qualified Data.Set as S
import Linear (V2(..), V3(..))

type Point = V3 Int
type Cube =  S.Set Point 

t1 :: [Char]
t1 = ".#.\n..#\n###"
t2 :: [Char]
t2 = ".###.#.#\n####.#.#\n#.....#.\n####....\n#...##.#\n########\n..#####.\n######.#"

initCube :: String -> S.Set Point 
initCube s =
  S.fromList        
      $ map fst
      $ filter snd
      $ concatMap
        (\ (y, xs) -> map (\ (x, c) -> (V3 x y 0, c == '#')) xs)
        (zip [0 .. ] $ map (zip [0 .. ]) $ lines s)

bounds :: Cube -> V3 (V2 Int)
bounds c =
  V3 xb yb zb
  where
    vs = S.toList c
    xb = let xs = map (\(V3 x _ _) -> x) vs in V2 (minimum xs) (maximum xs)
    yb = let ys = map (\(V3 _ y _) -> y) vs in V2 (minimum ys) (maximum ys)
    zb = let zs = map (\(V3 _ _ z) -> z) vs in V2 (minimum zs) (maximum zs)

neighbors :: Point -> [Point]
neighbors (V3 x y z) =
  [V3 x' y' z' | 
    x' <- [x-1 .. x+1],  
    y' <- [y-1 .. y+1], 
    z' <- [z-1 .. z+1], (x',y',z') /= (x,y,z)]

expand :: V3 (V2 Int) -> V3 (V2 Int)
expand (V3 (V2 x1 x2) (V2 y1 y2) (V2 z1 z2)) = 
  V3 (V2 (x1-1) (x2+1)) (V2 (y1-1) (y2+1)) (V2 (z1-1) (z2+1))

check :: Cube -> Point -> Bool
check c p =
  let l = length . filter id . map (`S.member` c) . neighbors $ p in
  if S.member p c then l == 2 || l == 3 else l == 3

step :: Cube -> Cube
step c = 
  foldl (\acc p -> if check c p then S.insert p acc else S.delete p acc) S.empty todo
  where 
    (V3 (V2 x1 x2) (V2 y1 y2) (V2 z1 z2)) = expand $ bounds c
    todo = [V3 x y z | x <- [x1..x2], y <- [y1..y2], z <- [z1..z2]]

run :: (Num a, Enum a) => a -> Cube -> Int
run n c = length $ foldl (\acc _ -> step acc) c [1..n]

solve20d17p1 :: IO ()
solve20d17p1 = print $ run 6 $ initCube t2
