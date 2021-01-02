
module Y20.Day20v2 () where

{-
ABANDONED ATTEMPT :/
-}

import Data.IntMap.Strict (IntMap(..), (!))
import qualified Data.IntMap.Strict as IM
import Data.List.Extra (sortOn, sortBy, sort, groupBy, intercalate, splitOn, transpose)
import Data.List (intersect)
import Util (applyN)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Debug.Trace (trace)

{------------------------
  DATA TYPES
-------------------------}

data Dir = N | E | S | W deriving (Show, Enum, Eq)

type Pos = (Int,Int)

data Panel = 
  Panel 
    { _num :: Int
    , _grid :: Grid
    , _sigs :: [Sig]
    , _placed :: Bool
    , _pos ::  Maybe Pos } deriving Show

type Sig = (Int,Bool,Dir)   -- (signature, N|S|E|W, flipped?)
type Grid = [[Bool]]
type Panels = IntMap Panel

{------------------------
  FILE NAMES
-------------------------}

fn1 :: [Char]
fn1 = "./Data/20/Day20.ex01.txt"
fn :: [Char]
fn = "./Data/20/Day20.txt"

{------------------------
  ROTATION and SIGNATURES
-------------------------}

-- | Rollover prev operator for @Dir@
dPred :: Dir -> Dir
dPred N = W
dPred d = pred d

-- | rotate CCW 90 degrees
rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

-- | mirror a grid
mirror :: [[a]] -> [[a]]
mirror = reverse . map reverse

-- | make a list of signatures. First 4 are NESW unflipped, then the same mirrored
sigs :: Grid -> [Sig]
sigs b =
  getSigs False b ++ getSigs True (mirror b)
  where
    getSigs :: Bool -> Grid -> [Sig]
    getSigs flipped bs =
      [ (getSig $ head bs, flipped, N)
      , (getSig $ map last bs, flipped, E)
      , (getSig $ last bs, flipped, S)
      , (getSig $ map head bs, flipped, W)]
    getSig = sum . zipWith (\n b -> (if b then 2^n else 0)) [0 .. ]

offset :: Dir -> (Int,Int) -> (Int,Int)
offset N (x,y) = (x,y-1)
offset E (x,y) = (x+1,y)
offset S (x,y) = (x,y+1)
offset W (x,y) = (x-1,y)

-- | "lock" a panel by creating a new grid for it in the given "flipped" state and
-- orientation, at the location given by the @(Dir, (Int,Int))@ tuple, and with a new
-- set of signatures for the locked in orientation.
-- The @offsetD@ indicates where this panel is relative to the coordinate @(x,y)@.
lock  
  :: Panel        -- ^ target panel being locked 
  -> Dir          -- ^ face of the target panel being connected
  -> Bool         -- ^ true if we need the mirror of the target
  -> Maybe        
    (Dir
    ,(Int,Int))   -- ^ if Just, defines the position of the panel the target is being connected
                  -- to, and the face the target is being connected to. The rotational difference
                  -- @rotDiff@ between this Dir and the target Dir is the number of CCW rotations
                  -- to move this panel to align with the panel it's being connected to
  -> Panel
lock p d f relPos = 
  p { _sigs = ss, _grid = adjustedGrid, _placed = True, _pos = Just pos }
  where
    (adjustedGrid,pos) = 
      let grid = _grid p in
      let flipG = if f then mirror grid else grid in
      case relPos of 
        Nothing -> (flipG,(0,0))
        Just (offD,offP) -> 
          let nr = rotDiff offD d in
          (applyN nr rotate flipG, offset offD offP)
    ss = take 4 . sigs $ adjustedGrid

rotDiff :: Dir -> Dir -> Int
rotDiff d1 d2 =
  go 0 d1 d2'
  where
    d2' = dPred . dPred $ d2
    go acc a b
      | a == b = acc
      | otherwise = go (acc+1) a (dPred b) 

{------------------------
  PARSING
-------------------------}

parseBlock :: String -> Panel
parseBlock raw =
  let grid = map (map (== '#')) ss in
  Panel { _num = num
        , _grid = grid
        , _sigs = sigs grid
        , _placed = False
        , _pos = Nothing }
  where
    (s:ss) = lines raw
    num = read . take 4 . drop 5 $ s

parseRaw :: String -> Panels
parseRaw raw =
  IM.fromList . map (\s -> let b = parseBlock s in (_num b, b)) 
  $ splitOn "\n\n" raw

{------------------------
  RENDERING
-------------------------}

-- | grid character from boolean value
b2ch :: Bool -> Char
b2ch b = if b then '#' else '.'

-- | Create a formatted render string of a panel
ppanel :: Panel -> IO ()
ppanel = putStrLn . intercalate "\n" . map (concatMap (\b -> if b then "██" else "  ")) . _grid

others :: Int -> Panels -> [Int]
others n ps = [o | o <- map fst . IM.toList $ ps, o /= n]

{------------------------
  MATCHING & IDENTIFYING
-------------------------}

-- | Identify the corners
corners :: IntMap Panel -> [(Int, Int)]
corners p = 
  filter ((==2) . snd) . map (\(n,pnl) -> (n, matchCount p n)) . IM.toList $ p

-- | Predicate - is pChild a match for p1?
match :: Panel -> Panel -> Bool
match p1 p2 = 
  let s1 = map (\(s,_,_)->s) . _sigs $ p1 in
  let s2 = map (\(s,_,_)->s) . _sigs $ p2 in
    (not . null) (s1 `intersect` s2) 

-- | Find the panels that match the given panel's non flipped orientation
findMatches :: Panel -> Panels -> [(Dir,(Int,Dir,Bool))]
findMatches p1 =
  concatMap (go . snd) . filter (\(n,_) -> n /= _num p1) . IM.toList
  where
    go p2 =
      [(d,(_num p2,d',f')) | 
        not . _placed $ p2,
        sig1@(s,f,d) <- _sigs p1, 
        sig2@(s',f',d') <- _sigs p2,           
          s == s', 
          not f]

arrange :: Panels -> [[(Int,(Int,Int))]]
arrange ps = 
  plg
  where
    getPos = fromMaybe (0,0) . _pos
    pl = sortOn (\(_,(x,y)) -> (y,x)) . map (\(_,p) -> (_num p, getPos p)) . IM.toList $ ps
    plg = groupBy (\(_,(_,y)) (_,(_,y')) -> y == y') pl

assemble :: Panels -> Panels
assemble panels =
  let corner = panels ! (fst . head . corners $ panels) in
  let lockedCorner = lock corner N False Nothing in
  let panels' = IM.insert (_num lockedCorner) lockedCorner panels in
  go panels' lockedCorner (findMatches lockedCorner panels')
  where
    go ps p1 [] = ps
    go ps pParent ((parentDir,(childNum,childDir,flipChild)):children) =
      let pChild = ps ! childNum in
      let parentPos = fromMaybe (0,0) (_pos pParent) in
      let pLockedChild = lock pChild childDir flipChild (Just (parentDir, parentPos)) in 
      let ps' = IM.insert (_num pLockedChild) pLockedChild ps in
      let nextMatches = findMatches pLockedChild ps' in
      let ps'' = go ps' pLockedChild nextMatches in
      go ps'' pParent children

-- | Count the number of matching panels for the given panel number
matchCount :: Panels -> Int -> Int
matchCount panels pnum = 
  length . filter id . map (match (panels IM.! pnum) . snd) . IM.toList . IM.delete pnum $ panels 

{------------------------
  SOLUTIONS
-------------------------}

solve20d20p1 :: FilePath -> IO ()
solve20d20p1 fn = do
  raw <- readFile fn
  let panels = parseRaw raw
  print $ product . map fst . corners $ panels

assembleF :: FilePath -> IO (Panels, Panels, [[(Int, (Int, Int))]])
assembleF fn = do
  raw <- readFile fn
  let ps = parseRaw raw 
  let ps' = assemble ps 
  let psa = arrange ps' in
    pure (ps, ps', psa)
  