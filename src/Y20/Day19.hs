module Y20.Day19 (solve20d19p1, solve20d19p2) where

import qualified Data.IntMap as M
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))
import Util ( isRight, numRight )
import Data.Maybe (catMaybes)

import Debug.Trace

type Rules = M.IntMap Rule
data Rule = Seq [M.Key]         -- ^ Must evaluate all rules in order
          | Or [M.Key] [M.Key]  -- ^ Must evaluate either left or right rules in order
          | Match Char          -- ^ Matches exactly one character
          deriving (Eq, Show)

fn :: String
fn = "./Data/20/Day19.txt"

parseMatchRule :: String -> Rule
parseMatchRule s = 
  let s' = s =~ "[a-z]" :: String
  in Match . head $ s'

parseIdList :: String -> [M.Key]
parseIdList = map read . filter (/="") . splitOn " " 

parseIdListRule :: String -> Rule
parseIdListRule = Seq . parseIdList

parseOrRule :: String -> Rule
parseOrRule s = 
  let [r1,r2] = splitOn "|" s
  in Or (parseIdList r1) (parseIdList r2)

parseRule :: String -> (M.Key, Rule)
parseRule s =
  (read id, rule)
  where
    [id, raw] = splitOn ":" s
    rule 
      | '|' `elem` raw = parseOrRule raw
      | '"' `elem` raw = parseMatchRule raw
      | otherwise      = parseIdListRule raw

parseRaw :: String -> (Rules, [String])
parseRaw s =
  let [s1,s2] = splitOn "\n\n" s in
  (M.fromList . map parseRule . splitOn "\n" $ s1, splitOn "\n" s2)

runTest :: Rules -> String -> Bool
runTest rm candidate =
  -- if any of the results is null (ie ""), that means it has no "leftovers", 
  -- then there's at least one path to a result for the candidate
  any null (go 0 [candidate])
  where
    go :: M.Key -> [String] -> [String]
    go _ [] = []
    go id ss =
      case rm M.! id of
        Match c      -> concatMap (runMatch c) ss      
        Seq ids      -> runSeq ids ss
        Or lIds rIds -> runOr lIds rIds ss
        
    runMatch :: Char -> String -> [String]
    runMatch _ [] = []
    runMatch c' (c:cs)
      | c' == c   = [cs]
      | otherwise = []

    runSeq :: [M.Key] -> [String] -> [String]
    runSeq [] ss = ss
    runSeq (id:ids) ss =
      case go id ss of
        [] -> []
        ss' -> runSeq ids ss'

    runOr :: [M.Key] -> [M.Key] -> [String] -> [String]
    runOr lIds rIds ss = runSeq lIds ss ++ runSeq rIds ss
            
part2Mod :: Rules -> Rules
part2Mod rm =
  let (k8,v8)   = parseRule "8: 42 | 42 8" in
  let (k11,v11) = parseRule "11: 42 31 | 42 11 31" in
  let rm' = M.insert k8 v8 rm in
  M.insert k11 v11 rm'

solve20d19p1 :: IO ()
solve20d19p1 = do
  raw <- readFile fn
  let (rules, tests) = parseRaw raw
  print $ length . filter id . map (runTest rules) $ tests

solve20d19p2 :: IO ()
solve20d19p2 = do
  raw <- readFile fn
  let (rm, tests) = parseRaw raw 
  print $ length . filter id . map (runTest (part2Mod rm)) $ tests  