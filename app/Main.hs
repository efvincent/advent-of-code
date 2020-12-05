module Main where
import qualified Y20.Day05
import Criterion.Main

main :: IO ()
main = defaultMain [
    bench "day05" $ nfIO Y20.Day05.solve5b
  ]
