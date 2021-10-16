module Main where
-- import Criterion.Main ( defaultMain, bench, bgroup, nfIO )
-- import Y20.Day19 ( solve20d19p1, solve20d19p2 )

-- main :: IO ()
-- main = defaultMain [
--   bgroup "Day 19" [ bench "part 1" $ nfIO solve20d19p1
--                   , bench "part 2" $ nfIO solve20d19p2
--                   ]
--   ]


import Y20.Day23v2 ( solve20d23p2, data20d32 )
import Y20.Day01

main :: IO ()
main = do {
  ans <- Y20.Day01.solve20d1p1 "/home/efvincent/code/aoc/data/20/Day01.txt" 2020;
  print ans
}