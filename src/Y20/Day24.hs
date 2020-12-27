module Y20.Day24 (solve20d24p2,solve20d24p1) where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import Util (numTrue, Point, applyN)
import Linear.V2 (V2(..))
import Data.Foldable (Foldable(foldl'))

type Tiles = Map Point Bool
data Bounds = Bounds 
  { _mins :: V2 Int
  , _maxs :: V2 Int}
    deriving Show

fn :: String
fn = "./Data/20/Day24.txt"

parseRaw :: String -> Tiles
parseRaw raw =
  let m = map (go (V2 0 0)) ss in
  foldl' (\acc k -> M.insertWith (\_ v -> not v) k True acc) M.empty m
  where
    ss = lines raw
    go p [] = p
    go (V2 x y) ('n':'e':cs) = go (V2 (x+1) (y-1)) cs
    go (V2 x y) ('s':'e':cs) = go (V2     x (y+1)) cs
    go (V2 x y) ('s':'w':cs) = go (V2 (x-1) (y+1)) cs
    go (V2 x y) ('n':'w':cs) = go (V2     x (y-1)) cs
    go (V2 x y) ('e':cs)     = go (V2 (x+1)     y) cs
    go (V2 x y) ('w':cs)     = go (V2 (x-1)     y) cs

bounds :: Tiles -> Bounds
bounds m =
  Bounds 
    { _mins = V2 (minimum xs - 1) (minimum ys - 1)
    , _maxs = V2 (maximum xs + 1) (maximum ys + 1) }
  where
    xs = map (\(V2 x _) -> x) $ M.keys m
    ys = map (\(V2 _ y) -> y) $ M.keys m

points :: Bounds -> [Point]
points Bounds{_mins=(V2 minx miny), _maxs=(V2 maxx maxy)} =
  [V2 x y | x <- [minx..maxx], y <- [miny..maxy]]

neighbors :: Tiles -> Point -> Int
neighbors m (V2 x y) = 
  numTrue $ [
    (\k -> M.findWithDefault False k m) (V2 (x + x') (y + y')) 
        | x' <- [-1, 0, 1], y' <- [-1, 0, 1], x' /= y']

play :: Tiles -> Tiles
play m =
  foldl' newState m allPoints
  where
    allPoints = points . bounds $ m
    newState acc p =
      let cn = neighbors m p in
      let ns = 
            if M.findWithDefault False p m 
              then not (cn == 0 || cn > 2) 
              else cn == 2 in
      if ns then M.insert p True acc else M.delete p acc

solve20d24p1 :: IO ()
solve20d24p1 = do
  raw <- readFile fn
  print . length . filter id . map snd . M.toList . parseRaw $ raw

solve20d24p2 :: IO ()
solve20d24p2 = do
  raw <- readFile fn
  let f = parseRaw raw in
    print . length . applyN 100 play $ f