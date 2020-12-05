module Y20.Day04 where
import           Data.List.Split (splitOn)
import           Text.Read       (readMaybe)
import           Text.Regex.TDFA ((=~))

-- | Required values
req :: [String]
req = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

heightrx :: String
heightrx = "\\`[0-9]+cm|in\\'"

eyeCol :: String
eyeCol = "\\`amb|blu|brn|gry|grn|hzl|oth\\'"

hairCol :: String
hairCol = "\\`#[0-9a-f]{6}\\'"

pid :: String
pid = "\\`[0-9]{9}\\'"

validateYear :: Int -> Int -> String -> Bool
validateYear mn mx s =
  all (\y -> y >= mn && y <= mx) $ readMaybe s

validateHeight :: String -> Bool
validateHeight s =
  s =~ heightrx &&
    let n = read $ s =~ "[0-9]+" in
      case s =~ "cm|in" :: String of
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

checkRequired :: [[String]] -> Bool
checkRequired s =
  let s' = map head s in
  all (`elem` s') req

checkRequiredAndValues :: [[String]] -> Bool
checkRequiredAndValues s =
  let s' = map (\[p1,p2] -> (p1,p2)) s in
  all (`elem` map fst s') req && all validateValue s'

solve :: ([[String]] -> Bool) -> String -> IO Int
solve algo filename = do
  raw <- readFile filename
  pure
    $ length
    $ filter (==True)
    $ map (algo . map (splitOn ":") . words) (splitOn "\n\n" raw)

{- USAGE:
λ> Y20.Day04.solve checkRequired "./data/20/Day04.txt"
λ> Y20.Day04.solve checkRequiredAndValues "./data/20/Day04.txt"
-}
