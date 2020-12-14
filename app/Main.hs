module Main where
import Criterion.Main ( defaultMain, bench, bgroup, nfIO )
import Y20.Day11v2 ( fn20d11 )

main :: IO ()
main = defaultMain [
  bgroup "Day 11" [ 
                  --, bench "part 2" $ nfIO $ Y20.Day10.solve20d10p2 fn20d10
                  ]
  ]
