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

xtra :: [Char]
xtra = "aaaabbaaaabbaaa"

xtra' :: [Either a ([Char], ([(Integer, Rule)], [Char]))]
xtra' = [Right (xtra,([(1,Match 'a'),(1,Match 'a'),(1,Match 'a'),(14,Match 'b'),(14,Match 'b'),(1,Match 'a'),(1,Match 'a'),(1,Match 'a'),(1,Match 'a'),(14,Match 'b'),(14,Match 'b'),(1,Match 'a'),(1,Match 'a'),(1,Match 'a'),(1,Match 'a')],""))]

(rm, tests) = parseRaw t2
rm' = modForP2 rm
s3 = solveV3 rm' tests

s2 :: [Either () (String, ([(Int, M.Key, Rule, String)], String))]
s2 = psolveV2 rm' tests
s2' = filter (\(Right (s,_)) -> s == "aaaabbaaaabbaaa") s2
s2'' :: [(Int, M.Key, Rule, String)]
Right (_, (s2'',_)) = head s2'

y = map (\(d, id, r, s) -> (take d $ cycle "..", id, r, s)) s2''

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

runTest :: Rules -> String -> Either () String
runTest rm s =
  case go 0 s of
    Right "" -> Right ""
    _ -> Left ()
  where

    runMatch :: Char -> String -> Either () String
    runMatch _ [] = Left ()
    runMatch c' (c:cs)
      | c' == c   = Right cs
      | otherwise = Left ()

    runSeq :: [M.Key] -> String -> Either () String
    runSeq [] s = Right s
    runSeq _ [] = Left ()
    runSeq (id:ids) s =
      case go id s of
        Right s' -> runSeq ids s'
        Left () -> Left ()

    trace' lefts rights = 
      if isRight lefts && isRight rights then
        let (Right ls) = lefts in
        let (Right rs) = rights in
        if length ls == length rs then "OK"
        else ("  *** " ++ show (length ls) ++ " vs " ++ show (length rs))
      else "OK"

    runOr :: [M.Key] -> [M.Key] -> String -> Either () String
    runOr lIds rIds s =
      let lefts  = runSeq lIds s in
      let rights = runSeq rIds s in
        if isRight (trace (trace' lefts rights) rights) then rights
        else if isRight lefts then lefts
        else Left ()

      -- case runSeq lIds s of
      --   Right s' -> Right s'
      --   Left () -> runSeq rIds s

    go _ [] = Left ()
    go id s =
      case rm M.! id of
        Match c      -> runMatch c s      
        Seq ids      -> runSeq ids s
        Or lIds rIds -> runOr lIds rIds s

test :: Rules -> String -> Either () (String,([(Int, M.Key,Rule,String)], String))
test rm candidate  =
  case go [] 0 0 candidate of
    Right r -> Right (candidate, r)
    Left () -> Left ()
  where
    matchSeq :: [(Int, M.Key,Rule,String)] -> Int -> [M.Key] -> String -> Either () ([(Int,M.Key,Rule,String)],String)
    matchSeq rs dp [] s = Right (rs,s) 
    matchSeq rs dp (x:xs) s = 
      case go rs (dp+1) x s of
        Right (rs, s') -> matchSeq rs (dp+1) xs s'
        Left () -> Left ()
    
    orSeq :: [(Int, M.Key,Rule,String)] -> Int -> [M.Key] -> [M.Key] -> String -> Either () ([(Int,M.Key,Rule,String)],String)
    orSeq rs dp s1 s2 s =
      if null s then Left () else
        case matchSeq rs (dp+1) s1 s of
          Left () -> matchSeq rs (dp+1) s2 s
          Right s' -> Right s'

    go :: [(Int, M.Key,Rule,String)] -> Int -> M.Key -> String -> Either () ([(Int,M.Key,Rule,String)],String)
    go rs dp _ [] = Right (rs,"")
    go rs dp id s@(c:cs) =
      case rm M.! id of
        Seq ids            -> case matchSeq rs (dp+1) ids s of 
                                    Right (results,s) -> Right ((dp, id, Seq ids,s):results, s); _ -> Left ()
        Or left right      -> case orSeq rs (dp+1) left right s of
                                    Right (results,s) -> Right ((dp, id, Or left right,s):results, s); _ -> Left ()
        Match c' | c' == c -> Right ((dp, id,Match c',s):rs, cs)
        Match c'           -> Left ()
                        
modForP2 :: Rules -> Rules
modForP2 rm =
  let (k8,v8)   = parseRule "8: 42 | 42 8" in
  let (k11,v11) = parseRule "11: 42 31 | 42 11 31" in
  let rm' = M.insert k8 v8 rm in
  M.insert k11 v11 rm'

solveV1 :: Rules -> [String] -> Int
solveV1 rm = length . filter id . map (=~ eval rm)

psolveV1 :: Rules -> [String] -> [String]
psolveV1 rm = filter (=~ eval rm)

solveV2 :: Rules -> [String] -> Int
solveV2 rm = length . filter (\t -> t /= Left ()) . map (test rm) 

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight  = not . isLeft

numTrue :: [Bool] -> Int
numTrue = length . filter id 

numRight :: [Either a b] -> Int
numRight = numTrue . map isRight

solveV3 :: Rules -> [String] -> Int
solveV3 rm = numRight . map (runTest rm)

psolveV3 :: Rules -> [String] -> [Either () String]
psolveV3 rm = filter isRight . map (runTest rm)

-- psolveV2 :: Rules -> [String] -> [Either () (String, [(Int, M.Key,Rule,String)], String)]
psolveV2 :: M.IntMap Rule -> [String] -> [Either () (String, ([(Int, M.Key,Rule,String)], String))]
psolveV2 rm tests = 
  j
  where
    m = map (test rm) tests
    j = filter (\t -> t /= Left ()) m

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
