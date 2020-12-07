module Y18.Day02 where

import           Data.Function   ((&))
import           Data.List.Extra (sort)
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as Set
import           Util            (histogram)

procLine :: [Char] -> Set.Set Int
procLine = Set.fromList . filter (>1) . map fst . histogram

solve1 :: [Char] -> Int
solve1 raw =
  (numOf [2] + twosAndThrees) * (numOf [3] + twosAndThrees)
  where
    counts = map (Set.toList . procLine) $ lines raw
    numOf x = length $ filter (== x) counts
    twosAndThrees = numOf [2,3]

solve18d2p1 :: IO ()
solve18d2p1 = do
  raw <- readFile "./data/18/Day02.txt"
  print $ solve1 raw

-- | Walks two strings of the same length. If they differ by exactly one,
-- drops that one and returns `Just string` where string is s (both are now
-- identical) without the differing char
drop1Comp :: String -> String -> Maybe String
drop1Comp s1 s2 =
  if length s1 /= length s2
    then Nothing
    else loop s1 s2 [] False
  where
    loop :: String -> String -> String -> Bool -> Maybe String
    loop []       []       r True             = Just (reverse r)
    loop (x1:x1s) (x2:x2s) r seen  | x1 == x2 = loop x1s x2s (x1:r) seen
    loop (x1:x1s) (x2:x2s) r False | x1 /= x2 = loop x1s x2s r True
    loop _ _ _ _ = Nothing

solve2 :: String -> String
solve2 s =
  lines s & sort & loop  & fromMaybe "no ans"
  where
    loop :: [String] -> Maybe String
    loop [] = Nothing
    loop (x:y:rest) = case drop1Comp x y of
      Just ans -> Just ans
      Nothing  -> loop (y:rest)

solve18d2p2 :: IO ()
solve18d2p2 = do
  raw <- readFile "./data/18/Day02.txt"
  print $ solve2 raw

{-
usage ::
λ> solve18d2p1
λ> solve18d2p2
-}
