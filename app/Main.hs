module Main where
import           Criterion.Main
import qualified Y20.Day06

main :: IO ()
main = defaultMain [
  bgroup "Day 6"  [ bench "part 1" $ nfIO Y20.Day06.solve61
                  , bench "part 2" $ nfIO Y20.Day06.solve62
                  ]
  ]
