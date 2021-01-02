module Y20.Day20v4 (solve20d20p1, solve20d20p2,fn20d20ex01,fn20d20) where

import Data.List.Extra (groupSortOn, intercalate, splitOn, transpose)
import Data.Map.Strict (Map, assocs, fromList, keys, member,
                        singleton, toList, union, (!))
import Data.Maybe      (catMaybes, fromMaybe, mapMaybe)
import Linear.V2       (V2 (..))
import Util            (applyN, countTrue, vfst, vsnd)

data Side = N | E | S | W deriving (Show,Ord,Eq)
type Grid = [[Bool]]
type Point = V2 Int
type Layout = Map Point Panel
type Panels = [Panel]
data Panel =
  Panel
    { _num  :: Int
    , _grid :: Grid
    , _loc  :: Point }
  deriving (Show, Eq)

{------------------------
  SOLUTIONS
-------------------------}

solve20d20p1 :: FilePath -> IO ()
solve20d20p1 fn = do
  raw <- readFile fn
  print . product . corners . assemble . parseRaw $ raw

solve20d20p2 :: FilePath -> IO ()
solve20d20p2 fn = do
  raw <- readFile fn
  print . roughness . combine . deborder . assemble . parseRaw $ raw

{------------------------
  DRAGON SCANNING
-------------------------}

roughness :: Grid -> Int
roughness g =
  tcount - (dc * dsz)
  where
    dc = dragonCount g
    dsz = sum . map length $ dragon
    tcount = sum . map (countTrue id) $ g

rawDragon :: [String]
rawDragon = ["                  # "
            ,"#    ##    ##    ###"
            ," #  #  #  #  #  #   "]

dragon :: [[Int]]
dragon =
  map (map fst . filter ((=='#') . snd) . zip [0..]) rawDragon

scanPoint :: Grid -> Point -> Bool
scanPoint g (V2 x y) =
      all (((g !! (y + 2)) !!) . (+ x)) (dragon !! 2)
  &&  all (((g !! (y + 1)) !!) . (+ x)) (dragon !! 1)
  &&  all (((g !! y)       !!) . (+ x)) (head dragon)

dragonCount :: Grid -> Int
dragonCount grid =
  head . take 1 . filter (> 0) . map go $ variants
  where
    variants = allVariants grid
    go :: Grid -> Int
    go g =
      length . filter id . map (scanPoint g) $ idxs
      where
        maxy = length g - 1 - (length rawDragon - 1)
        maxx = (length . head $ g) - 1  - ((length . head $ rawDragon) - 1)
        idxs = [V2 x y | y <- [0..maxy], x <- [0..maxx]]

{------------------------
  GRID BUILDING
-------------------------}

getSide :: Side -> Panel -> [Bool]
getSide N = head . _grid
getSide E = map last . _grid
getSide S = last . _grid
getSide W = map head . _grid

facing :: Side -> Side
facing N = S
facing E = W
facing S = N
facing W = E

getFacing :: Side -> Panel -> [Bool]
getFacing = getSide . facing

offset :: Map Side Point
offset = fromList [(N, V2 0 (-1)), (E, V2 1 0), (S, V2 0 1), (W, V2 (-1) 0)]

-- | rotate CCW 90 degrees
rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

rotations :: Grid -> [Grid]
rotations g = map (\n -> applyN n rotate g) [0..3]

flips :: Grid -> [Grid]
flips g = [g, reverse g, map reverse g]

allVariants :: Grid -> [Grid]
allVariants = concatMap flips . rotations

allPanelVariants :: Panel -> [Panel]
allPanelVariants p = map (\g -> p { _grid = g}) . allVariants . _grid $ p

findMatchFor :: Panels -> Panel -> Side -> Maybe Panel
findMatchFor panels panel s =
  go pl
  where
    np = _num panel
    loc = offset ! s + _loc panel
    pl = concatMap allPanelVariants . filter ((/=) np . _num) $ panels

    go [] = Nothing
    go (p:ps) =
      case isMatchFor s panel p of
        Nothing -> go ps
        Just p' -> Just $ p' { _loc = loc}

    isMatchFor :: Side -> Panel -> Panel -> Maybe Panel
    isMatchFor s p1 p2 =
      let s1 = getSide s p1 in
      let s2 = getFacing s p2 in
      if s1 == s2 then Just p2 else Nothing

assemble :: Panels -> Layout
assemble ps =
  go (singleton (V2 0 0) p1) p1
  where
    p1 = head ps
    go :: Layout -> Panel -> Layout
    go acc p =
      let ns = map (\p -> (_loc p, p)) . mapMaybe (findMatchFor ps p . fst) $ locs in
      let acc' = fromList ns `union` acc in
      foldl go acc' (map snd ns)
      where
        ploc = _loc p
        locs =
          filter (not . (`member` acc) . snd)
          . map (\(d,off) -> (d,off+ploc))
          . toList $ offset

corners :: Layout -> [Int]
corners ps =
  [ numOf minx miny, numOf minx maxy, numOf maxx miny, numOf maxx maxy ]
  where
    numOf x y = _num $ ps ! V2 x y
    xs = map vfst $ keys ps
    ys = map vsnd $ keys ps
    (minx, maxx) = (minimum xs, maximum xs)
    (miny, maxy) = (minimum ys, maximum ys)

deborder :: Layout -> Layout
deborder = fmap go
  where
    go :: Panel -> Panel
    go p =
      let clip = drop 1 . init in
      let grid' = map clip . clip . _grid $ p in
      p { _grid = grid' }

combine :: Layout -> Grid
combine =
  foldl1 (++)
    . map (foldl1 addGridsH . map (_grid . snd))
    . groupSortOn (vsnd . fst) . assocs
  where
    addGridsH :: Grid -> Grid -> Grid
    addGridsH = go []
      where
        go acc (r1:rs1) (r2:rs2) =
          let acc' = acc ++ [r1 ++ r2] in
          go acc' rs1 rs2
        go acc [] [] = acc

{------------------------
  PARSING
-------------------------}

parseBlock :: String -> Panel
parseBlock raw =
  let (s:ss) = lines raw in
  let num = read . take 4 . drop 5 $ s in
  let grid = map (map (== '#')) ss in
  Panel { _num = num
        , _grid = grid
        , _loc = V2 0 0 }

parseRaw :: String -> Panels
parseRaw raw =
  map parseBlock --(\s -> let b = parseBlock s in (_num b, b))
  $ splitOn "\n\n" raw

{------------------------
  RENDERING
-------------------------}

pgrid :: Grid -> IO ()
pgrid g =
  let s =
        intercalate "\n"
        . map (concatMap (\b -> if b then "██" else "  "))
        $ g in
  putStrLn $ s ++ "\n---------------------"

-- | Create a formatted render string of a panel
ppanel :: Panel -> IO ()
ppanel p = pgrid $ _grid p

{------------------------
  FILE NAMES
-------------------------}

fn20d20ex01 :: [Char]
fn20d20ex01 = "./Data/20/Day20.ex01.txt"
fn20d20 :: [Char]
fn20d20 = "./Data/20/Day20.txt"
