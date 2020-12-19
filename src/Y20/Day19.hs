module Y20.Day19 (solve20d19p1) where

import qualified Data.IntMap as M
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))

type Rules = M.IntMap Rule
data Rule = Seq [M.Key]
          | Or [M.Key] [M.Key]
          | Match Char
          deriving Show
          
t1 :: [Char]
t1 = "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"

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
    (id, raw) = case splitOn ":" s of [s1,s2] -> (s1,s2); _ -> error $ "Bad rule format: " ++ s
    rule 
      | '|' `elem` raw = parseOrRule raw
      | '"' `elem` raw = parseMatchRule raw
      | otherwise = parseIdListRule raw

parse :: String -> (M.IntMap Rule, [String])
parse s =
  (M.fromList . map parseRule . splitOn "\n" $ s1, splitOn "\n" s2)
  where
    [s1,s2] = splitOn "\n\n" s

eval :: M.IntMap Rule -> String
eval rm =  
  "\\`" ++ go 0 ++ "\\'"
  where
    go :: M.Key -> String
    go k =
      case rm M.! k of
        Match c -> [c]
        Seq ks -> foldl (\acc k -> acc ++ go k) "" ks
        Or lks rks -> "(" ++ (concat $ map go lks) ++ "|" ++ (concat $ map go rks) ++ ")"

solve20d19p1 :: FilePath -> IO ()
solve20d19p1 fn = do
  raw <- readFile fn
  let (rm, tests) = parse raw 
  let rx = eval rm 
  print $ length . filter id . map (\s -> s =~ rx :: Bool) $ tests


  


