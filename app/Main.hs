module Main where
import           Criterion.Main
import Y20.Day09 ( solve20d9p1, fn20d9, solve20d9p2 )

main :: IO ()
main = defaultMain [
  bgroup "Day 9"  [ bench "part 1" $ nfIO $ Y20.Day09.solve20d9p1 25 fn20d9
                  , bench "part 2" $ nfIO $ Y20.Day09.solve20d9p2 25 fn20d9
                  ]
  ]
