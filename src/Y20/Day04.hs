{-# LANGUAGE OverloadedStrings #-}

module Y20.Day04 where
import           Data.List.Split (splitOn)
import           Data.Text       (Text, pack, replace, unpack)
import           Text.Read       (readMaybe)
import           Text.Regex.TDFA ((=~))

-- | Replace fn, should really get used to using Text instead of string
repl :: Text -> Text -> String -> String
repl s1 s2 = unpack . replace s1 s2 . pack

-- | Required values
req :: [String]
req = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- | Could have used regexes to extract
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

validateYear :: Int -> Int -> String -> Bool
validateYear mn mx s =
  case readMaybe s::Maybe Int of
    Just y | length s == 4 && y >= mn && y <= mx -> True
    _                                            -> False

heightrx :: String
heightrx = "\\`[0-9]+cm|in\\'"

eyeCol :: String
eyeCol = "\\`amb|blu|brn|gry|grn|hzl|oth\\'"

hairCol :: String
hairCol = "\\`#[0-9a-f]{6}\\'"

pid :: String
pid = "\\`[0-9]{9}\\'"

validateHeight :: String -> Bool
validateHeight s =
  (s =~ heightrx :: Bool) &&
    let l = length s in
    let n = read (slice 0 (l - 3) s) :: Int
    in
      case slice (l - 2) l s of
        "in" -> n >= 59 && n <= 76
        "cm" -> n >= 150 && n <= 193
        _    -> False

validateValue :: (String, String) -> Bool
validateValue (k,v) =
  case k of
    "byr" -> validateYear 1920 2002 v
    "iyr" -> validateYear 2010 2020 v
    "eyr" -> validateYear 2020 2030 v
    "hgt" -> validateHeight v
    "hcl" -> v =~ hairCol
    "ecl" -> v =~ eyeCol
    "pid" -> v =~ pid
    _     -> True

checkRequired :: [String] -> Bool
checkRequired s =
  let s' = map (head . splitOn ":") s in
  all (`elem` s') req

checkRequiredAndValues :: [String] -> Bool
checkRequiredAndValues s =
  let s' = map ((\[p1,p2] -> (p1,p2)) . splitOn ":") s in
  all (`elem` map fst s') req && all validateValue s'

solve :: ([String] -> Bool) -> String -> IO Int
solve algo filename = do
  raw <- readFile filename
  let p2 =
        map (splitOn " ")
        $ splitOn "~"
        $ repl "\n" " "
        $ repl "\n\n" "~" raw in
    pure $ length $ filter (==True) $ map algo p2

{- USAGE:
λ> Y20.Day04.solve checkRequired "./data/20/Day04.txt"
λ> Y20.Day04.solve checkRequiredAndValues "./data/20/Day04.txt"
-}
