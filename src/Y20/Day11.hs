module Y20.Day11 (solve20d11p1v1) where

import Data.IntMap (IntMap, fromList, toList, (!), insert)

type X = Int
type Y = Int
type Point = (X,Y)
type Index = Int
data SeatState = Floor | Empty | Occupied deriving (Show, Eq)
data Seating = Seating
  { _maxx :: Int
  , _maxy :: Int
  , _seats :: IntMap SeatState }
  deriving (Show)

fn20d11v1 = "./data/20/Day11.txt"

fn20d11ex01v1 = "./data/20/Day11.ex01.txt"

decode :: Char -> SeatState
decode '.' = Floor
decode 'L' = Empty
decode '#' = Occupied

encode :: SeatState -> Char
encode Floor = '.'
encode Empty = 'L'
encode Occupied = '#'

render :: Seating -> String
render seating =
  reverse $ loop 0 ""
  where
    mx = _maxx seating
    seats = _seats seating
    l = length seats
    loop :: Int -> String -> String
    loop i s | i == l = s
    loop i s | i `mod` (mx+1) == 0 = loop (i+1) $ encode (seats!i) : '\n' : s
    loop i s = loop (i+1) $ encode (seats!i) : s

mkSeats :: String -> Seating
mkSeats s =
  Seating { _maxx = length (head raw) - 1, _maxy = length raw - 1, _seats = seats }
  where
    raw = lines s
    seats = fromList $ zip [0..] $ map decode $ concat raw

adjTo :: Int -> Point -> [Point]
adjTo mx (x,y) =  
  [(\ (dx, dy) -> (dx + x, dy + y)) (dx, dy) |
    dx <- [- 1 .. 1], 
    dy <- [- 1 .. 1], 
    not (dx == 0 && dy == 0) 
      && (dx + x >= 0)
      && (dx + x <= mx) 
      && (dy + y >= 0)]

fromPoint :: Int -> Point -> Index
fromPoint maxy (x,y) = x + (y * (maxy+1))

fromIdx :: Int -> Int -> Point
fromIdx maxx i =
  (x,y)
  where
    x = i `mod` (maxx+1)
    y = i `div` (maxx+1)

seatedAdj :: Seating -> Index -> Int
seatedAdj seating i =
  length 
    $ filter (== Occupied) 
    $ map (s!) 
    $ filter (< length s) 
    $ map (fromPoint $ _maxx seating) 
    $ adjTo mx . fromIdx mx
    $ i
  where
    mx = _maxx seating
    s = _seats seating

stepSeats :: Seating -> (Bool, Seating)
stepSeats seating = 
  loop False seating 0
  where
    len = length $ _seats seating

    loop :: Bool -> Seating -> Index -> (Bool, Seating)
    loop changed seats i | i == len = (changed, seats)
    loop changed seats i =
      if i > len then (changed, seats) else
      case _seats seats ! i of
        Empty | seatedAdj seating i == 0 -> 
          let seats' = seats { _seats = insert i Occupied (_seats seats)} in
          loop True seats' (i+1) 
        Occupied | seatedAdj seating i >= 4 ->
          let seats' = seats { _seats = insert i Empty    (_seats seats)} in
          loop True seats' (i+1) 
        _ -> loop changed seats (i+1)

runSeating :: Seating -> Seating
runSeating seating =
  case stepSeats seating of
    (True, s') -> runSeating s'        
    (False, s') -> s'

solve20d11p1v1 :: FilePath -> IO ()
solve20d11p1v1 fn = do
  raw <- readFile fn
  let s' = runSeating $ mkSeats raw
  let ans = length . filter (== Occupied) . map snd . toList . _seats $ s'
  print $ render s'
  print ans
