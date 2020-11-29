module Day02 where

type Memory = [Int]

memory :: Memory
memory = [] 

run :: (Int,Memory) -> (Int,Memory)
run pointer mem =
  mem
