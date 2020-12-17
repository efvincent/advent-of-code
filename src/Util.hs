module Util where

import Data.List.Split ( splitOn )
import Data.List (group, sort)
import Data.Foldable ( Foldable(toList) )
import Linear.V2(V2(..))
import Control.Monad (replicateM)

-- | Point used all over the place
type Point = V2 Int

-- | Remove an item from a list. Not efficient, use a map for 
-- performance intensive applications
removeItem :: Eq a => a -> [a] -> [a]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

-- | Generate a list of list of bits (using ints as bits, could / should use bools)
-- representing every permutation of bits at that depth
genBitPermutations :: Num a => Int -> [[a]]
genBitPermutations depth = 
  take (2^depth) 
  $ filter (\l -> length l == depth) 
  $ concatMap (`replicateM` [0,1]) [1..]

-- | Multiply a vector by an amount. This should work with normal (*), and does
-- in the repl, but doesn't in code ... don't know why, don't need to spend time
-- figuring it out, this works
(|*) :: Point -> Int -> Point
(|*) (V2 a b) n = V2 (a * n) (b * n)

-- | Like head but returns Nothing when list is empty
headMaybe :: [a] -> Maybe a
headMaybe p = if not (null p) then Just $ head p else Nothing

-- | Calls a function repeatedly with prev run's output until the output stops changing
stabilize :: Eq t => (t -> t) -> t -> t
stabilize f = 
  loop 
  where  
    loop  x
        | x == y    = x
        | otherwise = loop y
      where
        y = f x

-- | Call a function repeatedly, counting each call, until
-- the return value is the same as the last call. It then will
-- return the return value and the count of calls.
stabilizeAndCount :: (Eq a) => (a -> a) -> a -> (Int, a)
stabilizeAndCount f = 
  loop 1
  where  
    loop c x
        | x == y    = (c, x)
        | otherwise = loop (c+1) y
      where
        y = f x

-- | Count the number of items in a container where the predicate is true.
countTrue :: Foldable f => (a -> Bool) -> f a -> Int
countTrue p = length . filter p . toList

histogram :: Eq b => Ord b => [b] -> [(Int, b)]
histogram = map (\l -> (length l, head l)) . (group . sort)

-- | Runs a single test, by checking 'control' is equal to 'candidate'
-- Returns 'Either' where 'Right ()' is success, and 'Left msg' is the message supplied
-- in a failure case
check :: Eq a => String -> a -> a -> Either String ()
check msg control candidate =
  if control == candidate then Right () else Left msg

-- | Given a filename of a text file with one Int per line, returns 'IO [Int]' 
fileToIntList :: FilePath -> IO [Int]
fileToIntList filename = do
  raw <- readFile filename
  return $ map read $ words raw

-- | Given a file with a single line of comma separated Ints, return [Int]
fileOfCSVToIntList :: FilePath -> IO [Int]
fileOfCSVToIntList filename = do
  raw <- readFile filename
  return $ map read $ splitOn "," raw

-- | Given a filename of lines where each line is a comma separated list of strings,
-- return [[String]]
fileOfLinesOfCSVStringsToLists :: FilePath -> IO [[String]]
fileOfLinesOfCSVStringsToLists filename = do
  raw <- readFile filename
  return $ map (splitOn ",") $ lines raw