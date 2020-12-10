module Y20.Day10 (solve20d10p1, solve20d10p2, fn20d10) where

import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import qualified Data.IntMap as M
import qualified Data.IntSet as S
import Data.List.Extra (sort)

t1 :: [Char]
t1 = "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4"

fn20d10 :: [Char]
fn20d10 = "./Data/20/Day10.txt"

fn1e :: [Char]
fn1e = "./Data/20/Day10.ex01.txt"

s1 :: String -> Int
s1 s =
  n1 * n3
  where
    ns' = sort . map read . lines $ s
    ns = [0] ++ ns' ++ [last ns' + 3]
    (n1,n3,_) = foldl (\(n1,n3,last) n ->
      case n - last of
        0 -> (n1,n3  ,n)
        1 -> (n1+1,n3,n)
        3 -> (n1,n3+1,n)
        x -> error ("invalid case : " ++ show x)
      ) (0,0,0) ns

-- | Build a map of possible branches at a node
branches :: [Int] -> IntMap IntSet
branches ns = 
  -- Here the map's fromListWith will build a map from a list, but with a combining function
  -- that combines the values of the elements with the same key. Since the values are a Set, the
  -- applicative operator (<>) basically is like a generic combiner, for sets, it does a union.
  M.fromListWith (<>) nlist
  where
    ns' = 0:ns ++ [maximum ns + 3]
    nset = S.fromList ns'
    nlist = 
      [ (n, S.singleton candidates)
        | n <- ns'
          , candidates <- [n+1,n+2,n+3]
          , candidates `S.member` nset]

routesFromStart :: Num a => IntMap IntSet -> a
routesFromStart nset = 
  routesAtNode M.! 0
  where
    (final,_) = M.findMax nset
    routesAtNode = M.mapWithKey (\key branches ->
      if key == final -- last one will not have an entry in the map
        then 1  
        else sum . map (routesAtNode M.!) $ S.toList branches) nset

s2 :: String -> Int
s2 = routesFromStart . branches . sort . map read . lines

solve20d10p1 :: String -> IO ()
solve20d10p1 fn = do
  raw <- readFile fn
  print $ s1 raw

solve20d10p2 :: String -> IO ()
solve20d10p2 fn = do
  raw <- readFile fn
  print $ s2 raw  