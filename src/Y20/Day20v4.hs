module Y20.Day20v4 (solve20d20p2) where

import qualified Data.IntMap.Strict  as IM
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap(..), (!))
import Data.List.Extra (groupSortOn, foldl', intercalate, splitOn)
import Util (countTrue, applyN)
import Data.List (transpose)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Linear.V2 ( V2(..) )

data Side = N | E | S | W deriving (Show,Ord,Eq)
type Grid = [[Bool]]
type Point = V2 Int
type Layout = Map Point Panel
type Panels = IntMap Panel
data Panel =
  Panel
    { _num :: Int
    , _grid :: Grid 
    , _loc :: Point }
  deriving (Show, Eq)

{------------------------
  DRAGON SCANNING
-------------------------}

solve20d20p2 :: FilePath -> IO ()
solve20d20p2 fn = do
  raw <- readFile fn
  print . roughness . rawToGrid $ raw

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
    variants = allVariantsG grid
    go :: Grid -> Int
    go g =
      length . filter id . map (scanPoint g) $ idxs
      where
        -- max dims are length of the grid -1 as expected, but also subtract the length
        -- and width of the dragon less one as the dragon will not "wrap around" 
        maxy = length g - 1 - (length rawDragon - 1)
        maxx = (length . head $ g) - 1  - ((length . head $ rawDragon) - 1)
        idxs = [V2 x y | y <- [0..maxy], x <- [0..maxx]]

rotationsG :: Grid -> [Grid]
rotationsG g = map (\n -> applyN n rotate g) [0..3]

flipsG :: Grid -> [Grid]
flipsG g = [g, reverse g, map reverse g]

allVariantsG :: Grid -> [Grid]
allVariantsG = concatMap flipsG . rotationsG

{------------------------
  GRID BUILDING
-------------------------}

rawToGrid :: String -> Grid
rawToGrid = combine . deborder . assemble . parseRaw

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
offset = M.fromList [(N, V2 0 (-1)), (E, V2 1 0), (S, V2 0 1), (W, V2 (-1) 0)]

-- | rotate CCW 90 degrees
rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

rotations :: Panel -> [Panel]
rotations p = 
  let g = _grid p in
  map ( (\g' -> p {_grid = g'}) 
      . (\n -> applyN n rotate g)) 
      [0..3]

flips :: Panel -> [Panel]
flips p =
  let g = _grid p in 
    [ p
    , p { _grid = reverse g }
    , p { _grid = map reverse g } ]

allVariants :: Panel -> [Panel]
allVariants = concatMap flips . rotations 

findMatchFor :: Panels -> Panel -> Side -> Maybe Panel
findMatchFor panels panel s =
  go pl
  where
    np = _num panel
    loc = offset M.! s + _loc panel
    pl = concatMap allVariants . filter ((/=) np . _num) . map snd . IM.toList $ panels
    
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
  go (M.singleton (V2 0 0) p1) p1
  where
    p1 = snd . head . IM.toList $ ps
    go :: Layout -> Panel -> Layout
    go acc p =
      let ns = map (\p -> (_loc p, p)) . mapMaybe (findMatchFor ps p . fst) $ locs in
      let acc' = M.fromList ns `M.union` acc in
      foldl' go acc' (map snd ns)
      where
        ploc = _loc p
        locs = filter (not . (`M.member` acc) . snd) . map (\(d,off) -> (d,off+ploc)) . M.toList $ offset

deborder :: Layout -> Layout
deborder = M.map go
  where
    go :: Panel -> Panel
    go p =
      let clip = drop 1 . init in
      let grid' = map clip . clip . _grid $ p in
      p { _grid = grid' }

combine :: Layout -> Grid
combine ps =
  foldl' (++) (head rows) (tail rows)
  where
    groups = map (map (_grid . snd)) . groupSortOn (\(V2 _ y, _) -> y) . M.assocs $ ps
    foldRow r = foldl' addGridsH (head r) (tail r)
    rows = map foldRow groups
    addGridsH :: Grid -> Grid -> Grid
    addGridsH = 
      go []
      where
        go acc (r1:rs1) (r2:rs2) = let acc' = acc ++ [r1 ++ r2] in go acc' rs1 rs2
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
  IM.fromList . map (\s -> let b = parseBlock s in (_num b, b)) 
  $ splitOn "\n\n" raw

parseRawF :: FilePath -> IO Panels
parseRawF fn = do
  raw <- readFile fn
  pure $ parseRaw raw

getTileNums :: Panels -> [Int]
getTileNums = map fst . IM.toList

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

ppN :: Panels -> Int -> IO ()
ppN ps n = ppanel $ ps ! n

{------------------------
  FILE NAMES
-------------------------}

fn1 :: [Char]
fn1 = "./Data/20/Day20.ex01.txt"
fn :: [Char]
fn = "./Data/20/Day20.txt"