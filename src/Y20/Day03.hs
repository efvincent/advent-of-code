module Y20.Day03 (multiStepFile) where

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

  {- 
  This is an interesting, and typical, example of efficiencies gained
  from funtional composition. This line maps the `step` function (calculate 
  the result for a particular set of "trees" with a given slope) across all
  the slopes, generating a list of results:

      map (\s -> step trees width (0,0) s 0) slopes

  That list is then passed to a right fold over the `*` function, which multiples
  each result by an accumulator. This is the second pass through the list of
  slopes: 
  
      foldr (*) 1 $   ...result of map from above...
  
  This can be done in a single pass if we compose the muliply and the calling
  of the step function together. For each slope this composed function calculates
  the step result, then multiplies it by the accumulator.

  In this case, a single pass over such a short list is not really going to be
  a savings, but the practice is canonical Haskell, and building that brain-muscle
  memory so these cases are recognized automatically will yeild better quality
  solutions in the future ... that's what Advent of Code is all about (for me 🙂)
  -}
  pure $ foldr ((*) . (\ s -> step trees width (0, 0) s 0)) 1 slopes