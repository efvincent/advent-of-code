module Y20.Day03 where

-- Type definitions just make working with the type signatures easier
-- to read, and don't give any meaningful constraints beyond that
type Row = String
type Grid = [Row]
type Pos = (Int,Int)
type Slope = (Int,Int)
type Width = Int
type Count = Integer

-- | Step down the grid from top to bottom, skipping lines as dictated
-- by dy, wrapping x by the width of the grid. Note: don't increment x 
-- by dx when skipping a line. x should only get inc by dx once per slope, which
-- is dy number of lines.
step :: Grid -> Width -> Pos -> Slope -> Count -> Count
step [] _ _ _ n = n
step (tree:trees) width (x,y) (dx,dy) n =
  let x' = x `mod` width in
  let skipLine = y `mod` dy /= 0 in
  case tree!!x' of
    _ | skipLine -> step trees width (x,y+1)     (dx,dy) n
    '#'          -> step trees width (x'+dx,y+1) (dx,dy) (n+1)
    _            -> step trees width (x'+dx,y+1) (dx,dy) n

slopes1 :: [Slope]
slopes1 = [(3,1)]

slopes2 :: [Slope]
slopes2 = [(1,1),(3,1),(5,1),(7,1),(1,2)]

-- | Load the file and map over the step fn. Solves both parts 1 & 2
multiStepFile :: FilePath -> [Slope] -> IO Integer
multiStepFile fn slopes = do
  raw <- readFile fn
  let trees = words raw
  let width = length . head $ trees
  pure $ foldr (*) 1 $ map (\s -> step trees width (0,0) s 0) slopes