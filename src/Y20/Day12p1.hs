module Y20.Day12p1 (
  solve20d12p1,

  Op(..),
  parseOp,
  parseOps,
  dirToDelta,
  mDist) where

import           Linear.V2 (V2 (..))
import           Util      (Point, (|*))

-- | A compass direction. Wrap around inc and dec with `dPred` and `dSucc`
data Dir = E | S | W | N deriving (Show, Eq, Ord, Enum, Read)

-- | An operation for this exercise. `L`eft, `R`ight, `F`orward, `Move` 
-- in `Dir`ection, and then an Int argument for each
data Op = L Int
        | R Int
        | F Int
        | Move Dir Int
        deriving (Show, Eq, Ord)

data State = State
  { _dir :: Dir
  , _loc :: Point }
  deriving (Show)

initState = State { _dir = E, _loc = V2 0 0 }

-- | Parse the string operation into the `Op`
parseOp :: [Char] -> Op
parseOp (c:cs) =
  case c of
    'N' -> Move N v
    'S' -> Move S v
    'E' -> Move E v
    'W' -> Move W v
    'F' -> F v
    'L' -> L v
    'R' -> R v
  where
    v = read cs

-- | Parse the exercise file (multiple lines) into 
-- a list of operations
parseOps :: String -> [Op]
parseOps = map parseOp . lines

turn :: (Dir -> Dir) -> Int -> Dir -> Dir
turn fn n = (!! (n `div` 90)) . iterate fn

dPred :: Dir -> Dir
dPred E = N
dPred d = pred d

dSucc :: Dir -> Dir
dSucc N = E
dSucc d = succ d

right :: Int -> Dir -> Dir
right = turn dSucc

left :: Int -> Dir -> Dir
left = turn dPred

dirToDelta :: Num a => Dir -> V2 a
dirToDelta E = V2 1 0
dirToDelta S = V2 0 (-1)
dirToDelta W = V2 (-1) 0
dirToDelta N = V2 0 1

execOp :: State -> Op -> State
execOp st@State{_dir=d,_loc=l} op =
  case op of
    Move m v -> st { _loc = l + (dirToDelta m |* v)}
    F v      -> st { _loc = l + (dirToDelta d |* v)}
    L v      -> st { _dir = left  v d }
    R v      -> st { _dir = right v d }

runOps :: Foldable t => t Op -> State
runOps = foldl execOp initState

mDist :: Num a => V2 a -> a
mDist (V2 a b) = abs a + abs b

solve20d12p1 :: IO ()
solve20d12p1 = do
  raw <- readFile "./data/20/Day12.txt"
  let ans = runOps $ parseOps raw
  print $ mDist $ _loc ans
