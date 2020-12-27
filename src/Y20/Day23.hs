module Y20.Day23 (solve20d23p1) where

import qualified  Data.CircularList as CL
import Data.CircularList (rotL, insertL, rotNR, rotNL, rotR, removeR, insertR, focus, rotateTo)
import Debug.Trace
import Data.Traversable (for)
import Data.Foldable (Foldable(foldl'))

type Cups = CL.CList Int

t :: [Char]
t = "583976241"
t1 :: [Char]
t1 = "389125467"

parseRaw :: [Char] -> Cups
parseRaw = CL.fromList . map (read . (:[])) 

parseRaw2 :: [Char] -> Cups
parseRaw2 s = 
  let xs = map (read . (:[])) s in
  let maxx = maximum xs in
  CL.fromList $ xs ++ [(maxx + 1) .. 1000000]

popR :: CL.CList a -> (Maybe a, CL.CList a)
popR cl = (focus cl, removeR cl)

pickUp :: Int -> Cups -> ([Int], Cups)
pickUp c cups = 
  let Just (hand, cups') = go [] c (rotR cups) in
    (reverse hand, cups')
  where
    go acc 0 cl = Just (acc, cl)
    go acc c cl = do
      n <- focus cl 
      let cl' = removeR cl
      go (n:acc) (c-1) cl'

curOf :: CL.CList a -> a
curOf cups = let Just cur = focus cups in cur

findDest 
  :: Int        -- ^ minumum value. If we're below here, wrap to max 
  -> Int        -- ^ max value. Wrap here if target is below min
  -> Int        -- ^ target we're looking for
  -> [Int]      -- ^ cups in hand - target cannot be these
  -> Cups       -- ^ the current circle of cups; we'll be rotating them in recursive calls
  -> Maybe Cups -- ^ finally the configuration when we're on target
findDest mn mx target hand cups
  | target < mn        = findDest mn mx mx         hand cups
  | target `elem` hand = findDest mn mx (target-1) hand cups 
  | otherwise =
    case rotateTo target cups of
      Nothing -> findDest mn mx (target -1) hand cups
      ans -> ans
  
push :: Cups -> [Int] -> Cups
push cups xs = 
  let cups' = foldl (\acc x -> rotL $ insertL x acc) cups xs in
  cups'

turn :: Int -> Int -> Cups -> Cups
turn minN maxN cups = 
  cups'
  where
    cur = curOf cups
    (hand, cups') = 
      let (h,cs) = pickUp 3 cups in
      let target = cur - 1 in
      let Just dest = findDest minN maxN target h cs in
      let inserted = insertL (hand !! 2) . insertL (hand !! 1) . insertL (head hand) $ dest in
      let Just backToCur = rotateTo cur inserted in
      (h,rotR backToCur)

takeNturns :: (Enum a, Num a) => a -> Cups -> Cups
takeNturns n cups = 
  foldl' (\acc _ -> turn minN maxN acc) cups [1..n]
  where
    minN = minimum cups
    maxN = maximum cups

solve20d23p1 :: Int -> String -> IO ()
solve20d23p1 n raw = 
  let Just cups' = rotateTo 1 . takeNturns n . parseRaw $ raw in
  print . concatMap show . tail . CL.toList $ cups'
