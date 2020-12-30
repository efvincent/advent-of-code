module Y20.Day25 (solve20d25) where

import Util (applyN)
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap, (!?))
import Data.List (find)
import Data.Foldable (Foldable(foldl'))
import Math.NumberTheory.Powers.Modular (powModInt)

xs :: (Int,Int)
xs = (\[pk1,pk2] -> (pk1,pk2)) . map read . lines $ "14205034\n18047856" 

-- | compute the private key given two public keys per puzzle algo.
-- Key function: @powModInt b e m@ computes @(b^e) `mod` m@ efficiently. For my puzzle, the formula
-- would have been @18047856 ^ 10646957 `mod` 20201227@ which takes over 2s, but with @powModInt@ it's 
-- running in less than 0.01s.
--
-- Using the formula @pk ^ otherLoopSize `mod` 20201227@ is a hint I found on reddit. The alternative
-- would be to run the loop @otherLoopSize@ times, which would work fine and not take too long, but this
-- approach gets the final solution in about 5s
--
-- The bulk of the function is spent in the iterate function, looking for lower of the two loop
-- counts that fit the two public keys.
getPrivateKey :: (Int,Int) -> Int
getPrivateKey (pk1, pk2) = 
  powModInt otherPk loopSize 20201227
  where
    (loopSize, foundPk) =                   -- this yeilds the loop size
      head $                                -- iterate will continue looking, but we need just the first
      filter ((\p -> p == pk1 || p == pk2) . snd) $   -- look for either public key
      zip [0..] $                           -- will create a tuple with the loop size
      iterate ((`mod` 20201227) . (* 7)) 1  -- iterate feeds the resunt of the fn back to itself while yielding it
    otherPk = if foundPk /= pk1 then pk1 else pk2

solve20d25 :: IO ()
solve20d25 = print $ getPrivateKey xs