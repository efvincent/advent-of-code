module Y20.Day19 where --  (solve20d19p1) where

import qualified Data.IntMap as M
import Data.List.Split (splitOn)
import Text.Regex.TDFA ((=~))
import Debug.Trace

type Rules = M.IntMap Rule
data Rule = Seq [M.Key]
          | Or [M.Key] [M.Key]
          | Match Char
          deriving (Eq, Show)

fn :: [Char]
fn = "./Data/20/Day19.txt"

t1 :: [Char]
t1 = "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb"

t2 :: [Char]
t2 = "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"

a2 :: [Char]
a2 = "bbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"

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

parseRaw :: String -> (Rules, [String])
parseRaw s =
  let [s1,s2] = splitOn "\n\n" s in
  (M.fromList . map parseRule . splitOn "\n" $ s1, splitOn "\n" s2)

eval :: Rules -> String
eval rm =  
  "\\`" ++ go 0 ++ "\\'"
  where
    go :: M.Key -> String
    go k = case rm M.! k of
        Match c    -> [c]
        Seq ks     -> foldl (\acc k -> acc ++ go k) "" ks
        Or lks rks -> "(" ++ concatMap go lks ++ "|" ++ concatMap go rks ++ ")"

pall :: Show a => [a] -> IO ()
pall [] = return ()
pall (x:xs) = do
  print x
  pall xs

test :: Rules -> String -> Either () ([Rule], String)
test rm candidate =
  go [] 0 candidate 
  where
    matchSeq :: [Rule] -> [M.Key] -> String -> Either () ([Rule],String)
    matchSeq rs [] s = Right (rs,s) 
    matchSeq rs (x:xs) s = 
      case go rs x s of
        Right (rs, s') -> matchSeq rs xs s'
        Left () -> Left ()
    
    orSeq :: [Rule] -> [M.Key] -> [M.Key] -> String -> Either () ([Rule], String)
    orSeq rs s1 s2 s =
      case matchSeq rs s1 s of
        Left () -> matchSeq rs s2 s
        Right s' -> Right s'

    go :: [Rule] -> M.Key -> String -> Either () ([Rule],String)
    go rs _ [] = Right (rs,"")
    go rs id s@(c:cs) =
      case rm M.! id of
        Seq ids            -> matchSeq rs ids s
        Or left right      -> orSeq rs left right s
        Match c' | c' == c -> Right (Match c':rs, cs)
        Match c'           -> Left ()
                        
modForP2 :: Rules -> Rules
modForP2 rm =
  let (k8,v8)   = parseRule "8: 42 | 42 8" in
  let (k11,v11) = parseRule "11: 42 31 | 42 11 31" in
  let rm' = M.insert k8 v8 rm in
  M.insert k11 v11 rm'

solveV1 :: Rules -> [String] -> Int
solveV1 rm = length . filter id . map (=~ eval rm)

solveV2 :: Rules -> [String] -> Int
solveV2 rm = length . filter (\t -> t /= Left ()) . map (test rm) 

psolveV2 :: Rules -> [String] -> [Either () ([Rule], String)]
psolveV2 rm = filter (\t -> t /= Left ()) . map (test rm)

solve20d19p1 :: FilePath -> IO ()
solve20d19p1 fn = do
  raw <- readFile fn
  print $ uncurry solveV1 $ parseRaw raw

-- solve20d19p1v2 :: FilePath -> IO ()
-- solve20d19p1v2 fn = do
--   raw <- readFile fn
--   let (rm, tests) = parseRaw raw 
--   print $ length . filter id . map (test rm) $ tests

-- solve20d19p2v2 :: FilePath -> IO ()
-- solve20d19p2v2 fn = do
--   raw <- readFile fn
--   let (rm, tests) = parseRaw raw 
--   print $ length . filter id . map (test (modForP2 rm)) $ tests  
