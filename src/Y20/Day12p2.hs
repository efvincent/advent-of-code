module Y20.Day12p2 (solve20d12p2) where

import Y20.Day12p1 ( Op(..), dirToDelta, mDist, parseOps )
import Linear.V2 ( V2(..) )
import Util ((|*), Point)

data State = State
  { _loc :: Point
  , _way :: Point }
  deriving (Show)

initState :: State
initState = State { _loc = V2 0 0, _way = V2 10 1 }

turn :: (Point -> Point) -> Int -> Point -> Point
turn fn n = (!! (n `div` 90)) . iterate fn

right :: Int -> Point -> Point
right = turn (\(V2 x y) -> V2 y (-x))

left :: Int -> Point -> Point
left = turn (\(V2 x y) -> V2 (-y) x)

execOp :: State -> Op -> State
execOp st@State{_loc=l,_way=way} op =
  case op of
    Move m v -> st { _way = way + (dirToDelta m |* v) }
    F v      -> st { _loc = l + (way |* v) }
    L v      -> st { _way = left v way }
    R v      -> st { _way = right v way }

runOps :: Foldable t => t Op -> State
runOps = foldl execOp initState

solve20d12p2 :: IO ()
solve20d12p2 = do
  raw <- readFile "./data/20/Day12.txt"
  let ans = runOps $ parseOps raw
  print $ mDist $ _loc ans