module Y20.Day19 (solve20d19p1) where

import qualified Data.IntMap as M
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

type Rules = M.IntMap Rule
data Rule = Seq [M.Key]
          | Or [M.Key] [M.Key]
          | Match Char
          deriving Show

parseMatchRule :: String -> Rule
parseMatchRule s = 
  let s' = s =~ "[a-z]" :: String
  in Match . head $ s'

parseIdList :: [Char] -> [M.Key]
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

parse :: String -> (M.IntMap Rule, [String])
parse s =
  let [s1,s2] = splitOn "\n\n" s in
  (M.fromList . map parseRule . splitOn "\n" $ s1, splitOn "\n" s2)

eval :: M.IntMap Rule -> String
eval rm =  
  "\\`" ++ go 0 ++ "\\'"
  where
    go :: M.Key -> String
    go k = case rm M.! k of
        Match c    -> [c]
        Seq ks     -> foldl (\acc k -> acc ++ go k) "" ks
        Or lks rks -> "(" ++ concatMap go lks ++ "|" ++ concatMap go rks ++ ")"

solve20d19p1 :: FilePath -> IO ()
solve20d19p1 fn = do
  raw <- readFile fn
  let (rm, tests) = parse raw 
  let rx = eval rm 
  print $ length . filter id . map (=~ rx) $ tests
