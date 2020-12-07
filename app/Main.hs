module Main where
import           Criterion.Main
import Y20.Day07 ( solve20d7p1, fn20d7, solve20d7p2 )

main :: IO ()
main = defaultMain [
  bgroup "Day 7"  [ bench "part 1" $ nfIO $ Y20.Day07.solve20d7p1 fn20d7
                  , bench "part 2" $ nfIO $ Y20.Day07.solve20d7p2 fn20d7
                  ]
  ]
