module AOC2020.Day01 where

import Util ( fileToIntList )

findPairsThatSumTo :: Int -> [Int] -> Int
findPairsThatSumTo v ns =
  case [(n1,n2) | n1 <- ns, n2 <- ns, n1+n2 == v] of
    [] -> 0
    ((n1,n2):_) -> n1 * n2

findTripsThatSumTo :: Int -> [Int] -> Int
findTripsThatSumTo v ns =
  case [(n1,n2,n3) | n1 <- ns, n2 <- ns, n3 <- ns, n1+n2+n3 == v] of
    [] -> 0
    ((n1,n2,n3):_) -> n1 * n2 * n3

solve :: FilePath -> Int -> IO Int
solve fn v = do
  ns <- fileToIntList fn
  return $ findPairsThatSumTo v ns

solve2 :: FilePath -> Int -> IO Int
solve2 fn v = do
  ns <- fileToIntList fn
  return $ findTripsThatSumTo v ns
