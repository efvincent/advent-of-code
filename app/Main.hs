module Main where
import Criterion.Main ( defaultMain, bench, bgroup, nfIO )
import Y20.Day19 ( solve20d19p1, solve20d19p2 )

main :: IO ()
main = defaultMain [
  bgroup "Day 19" [ bench "part 1" $ nfIO solve20d19p1
                  , bench "part 2" $ nfIO solve20d19p2
                  ]
  ]
