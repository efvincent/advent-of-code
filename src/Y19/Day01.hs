module Y19.Day01 where

import Util (fileToIntList, check)

fuelFor :: Int -> Int
fuelFor mass = (mass `div` 3) - 2

deepFuelFor :: Int -> Int
deepFuelFor n | n <= 0 = 0
deepFuelFor n = let req = fuelFor n in
  (if req < 0 then 0 else req) + deepFuelFor req

tests :: Either String ()
tests =
  run 2 12
  >> run 2 14
  >> run 654 1969
  >> run 33583 100756
  where
    run control tval = 
      check ("mass " ++ show tval ++ " should yield " ++ show control) control 
      $ fuelFor tval

solve :: (Int -> Int) -> [Int] -> Int
solve fn xs = sum [fn x | x <- xs]

ans :: (Int -> Int) -> FilePath -> IO Int
ans fn filename = do
  xs <- fileToIntList filename
  return $ solve fn xs