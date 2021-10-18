module Y20.Day07 (solve20d7p1,solve20d7p2,fn20d7) where

import           Data.Function   ((&))
import           Data.List.Extra (splitOn)
import           Data.Map.Strict (Map, fromList, toList, (!))
import qualified Data.Map.Strict as M (empty, insert, lookup)
import           Data.Maybe      (mapMaybe)

type BagId     = String
type Rule      = (Int, BagId)
type Rules     = Map BagId [Rule]
type Content   = (BagId, [Rule])
type HoldCount = Map BagId Int

fn20d7ex1 :: [Char]
fn20d7ex1 = "./data/20/Day07.ex01.txt"

fn20d7ex2 :: [Char]
fn20d7ex2 = "./data/20/Day07.ex02.txt"

fn20d7 :: [Char]
fn20d7 = "./data/20/Day07.txt"

-- | Parses the portion of the rule that specifies a contained bag and its quantity
parseChild :: String -> Maybe Rule
parseChild s =
  if take 2 s == "no" then Nothing
    else 
      let [quant, adj, col, _] = words s in
      Just (read quant, adj ++ col)

-- | Parses one of the bag rules in the form:
-- "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
-- and parses it into `Content`, which is a tuple of the bag ID "muted yellow"
-- and a list of `Rule` each of which is a count of `BagId`
parseBagRule :: String -> Content
parseBagRule bagRuleText =
  (adj ++ col,rules)
  where
    [b,cs] = splitOn " contain " bagRuleText
    [adj, col, _] = words b
    rules = mapMaybe parseChild $ splitOn ", " (take (length cs - 1) cs)

mkRules :: String -> Rules
mkRules s =  fromList $ map parseBagRule $ lines s

-- | Returns true if the 2nd bagId is a descendant of the first
isDescendent :: Rules -> BagId -> BagId -> Bool
isDescendent rules target candidate =
  let children = map snd $ rules ! candidate in
  elem target children || any (isDescendent rules target) children

-- | works by maintaining a "hold count", which is a `Map` from `BagId` to `Int` which
-- is the count of bags in a bag. This map is passed as part of the accumulator in a 
-- fold that follows the bag contents hierarchy specified in `rules`. Whenever a child's
-- count is found it's not recalculated. If it's not found, its calculated and inserted
-- into the hold count to create a new one, which is then threaded through the rest 
-- of the fold
countContents :: Rules -> HoldCount -> BagId -> (HoldCount, Int)
countContents rules hCount target =
  let children = rules ! target in
  foldl (\(hc,acc) (childQuant, childId) ->
    case childId `M.lookup` hc of
      Just childContents -> (hc, (childQuant * (1 + childContents)) + acc)
      Nothing ->
        let (hc', childContents) = countContents rules hc childId in
        (M.insert childId childContents hc', (childQuant * (1 + childContents)) + acc))
    (hCount,0) children

solve20d7p1 :: String -> IO ()
solve20d7p1 fn = do
  raw <- readFile fn
  let rules = mkRules raw
  let bagIds = map fst $ toList rules
  print $ map (isDescendent rules "shinygold") bagIds & filter (==True) & length

solve20d7p2 :: String -> IO ()
solve20d7p2 fn = do
  raw <- readFile fn
  let rules = mkRules raw
  print $ snd $ countContents rules M.empty "shinygold"
