module Main where
import Criterion.Main ( defaultMain, bench, bgroup, nfIO )
import Y20.Day10 ( solve20d10p1, fn20d10, solve20d10p2 )

main :: IO ()
main = defaultMain [
  bgroup "Day 10" [ bench "part 1" $ nfIO $ Y20.Day10.solve20d10p1 fn20d10
                  , bench "part 2" $ nfIO $ Y20.Day10.solve20d10p2 fn20d10
                  ]
  ]
