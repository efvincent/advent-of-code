module Y20.Day01 (solve20d1p1,solve20d1p2) where

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

solve20d1p1 :: FilePath -> Int -> IO Int
solve20d1p1 fn v = do
  ns <- fileToIntList fn
  return $ findPairsThatSumTo v ns

solve20d1p2 :: FilePath -> Int -> IO Int
solve20d1p2 fn v = do
  ns <- fileToIntList fn
  return $ findTripsThatSumTo v ns
