module Main where
import           Criterion.Main
import Y20.Day08 ( solve20d8p1, fn20d8, solve20d8p2 )

main :: IO ()
main = defaultMain [
  bgroup "Day 8"  [ bench "part 1" $ nfIO $ Y20.Day08.solve20d8p1 fn20d8
                  , bench "part 2" $ nfIO $ Y20.Day08.solve20d8p2 fn20d8
                  ]
  ]
