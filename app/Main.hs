module Main where
-- import Criterion.Main ( defaultMain, bench, bgroup, nfIO )
-- import Y20.Day19 ( solve20d19p1, solve20d19p2 )

-- main :: IO ()
-- main = defaultMain [
--   bgroup "Day 19" [ bench "part 1" $ nfIO solve20d19p1
--                   , bench "part 2" $ nfIO solve20d19p2
--                   ]
--   ]


import Y20.Day23v2

main :: IO ()
main = do {
  Y20.Day23v2.solve20d23p2 1000000 10000000 "394618527" -- solve20d2 "/home/efvincent/code/aoc/data/20/Day01.txt";
  
}