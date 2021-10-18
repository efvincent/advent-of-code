module Y20.Day16 (solve20d16p1, solve20d16p2) where

import Data.List.Split (splitOn)
import Data.List.Extra (transpose, sortOn, isPrefixOf)
import Data.Bifunctor (second) 
import Util ( removeItem )

type Field = String
type Range = (Int,Int)
type Rule = (Field, (Range, Range))
type Ticket = [Int]
data Params = Params
  { _rules :: [Rule]
  , _ticket :: Ticket
  , _others :: [Ticket] }
  deriving Show

fn = "./data/20/Day16.txt"

parseRules :: String -> [Rule]
parseRules = 
  map parseRule . splitOn "\n"
  where
    parseRule :: String -> Rule
    parseRule s =
      (field,(r1,r2))
      where
        [field,s2] = splitOn ": " s
        rawRanges = splitOn " or " s2
        [r1,r2] = map parseRange rawRanges
        parseRange :: String -> (Int,Int)
        parseRange r = 
          let [r1, r2] = map read $ splitOn "-" r
          in (r1,r2)

parse :: String -> Params
parse s =
  let [s1,s2,s3] = splitOn "\n\n" s 
  in Params 
      { _rules = parseRules s1
      , _ticket = map read . splitOn "," . ((!! 1) . splitOn "\n") $ s2
      , _others = map (map read . splitOn ",") . tail . splitOn "\n" $ s3 }

isValid :: Int -> Range -> Bool
isValid i (r1, r2) = r1 <= i && i <= r2

isValidField :: [Rule] -> Int -> Bool
isValidField rs i =
  any (\(_,(r1,r2)) -> isValid i r1 || isValid i r2) rs

isValidTicket :: Params -> Ticket -> Bool
isValidTicket p = 
  let rs = _rules p in
  all (isValidField rs)

tossInvalid :: Params -> Params
tossInvalid p@Params{_others=o} = p {_others= filter (isValidTicket p) o}

-- | returns true if a rule is valid for all values
isValidRule :: Rule -> [Int] -> Bool
isValidRule (_,(r1, r2)) = all (\x -> isValid x r1 || isValid x r2) 

-- errRate :: Params -> Int
errRate :: Params -> Int
errRate Params{_others=o, _rules=rs} =
  sum . filter (not . isValidField rs) . concat $ o

-- | Folding function to takes an (index,[values]) and appends one (index,[Field])
-- to the accumulator, the set of all possible fields for an index 
-- given the values for that index.
getPossibiles :: [Rule] -> [(Int, [Field])] -> (Int, [Int]) -> [(Int, [Field])]
getPossibiles rules acc (idx,vals) = 
  (idx, map fst $ filter (`isValidRule` vals) rules) : acc

toSingle :: [(Int,[Field])] -> [(Int,[Field])] -> [(Int,[Field])]
toSingle acc [] = acc
toSingle acc (single@(_, field:_):rest) = 
    toSingle (single : acc) $ map (second (removeItem field)) rest 

solve :: Params -> Int
solve p@Params{_ticket=t} =
  product $ map ((t!!) . fst) $ filter (isPrefixOf "departure" . head . snd) 
  $ toSingle [] $ sortOn (length . snd) 
  $ foldl (getPossibiles (_rules p)) [] 
  $ zip [0..] $ transpose $ t : _others (tossInvalid p)
  
solve20d16p1 :: IO ()
solve20d16p1 = do
  raw <- readFile fn
  print $ errRate $ parse raw

solve20d16p2 :: IO ()
solve20d16p2 = do
  raw <- readFile fn
  print $ solve $ parse raw