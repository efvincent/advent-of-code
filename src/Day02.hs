module Day02 where

import Data.IntMap.Strict
import Control.Monad.State
import Data.Maybe
import Util (fileOfCSVToIntList)
import Debug.Trace

type Memory = IntMap Int
type MemValue = Int
type Address = Int

data RunState = Running
              | InputWait
              | Halted
              | Fault String
  deriving (Show)
data Computer = Computer
  { _memory :: Memory 
  , _curAddr :: Address
  , _runState :: RunState }
  deriving (Show)


data Instruction  = Halt
                  | Add { v1addr :: Address, v2addr :: Address, resultAddr :: Address }
                  | Multiply { v1addr :: Address, v2addr :: Address, resultAddr :: Address }
                  | Other MemValue
  deriving (Show)

-- | gets the memory at the location, returning zero if it doesn't exist
getMemAt :: Computer -> Address -> MemValue
getMemAt comp addr = fromMaybe 0 $ _memory comp !? addr 

decodeOp :: Computer -> Instruction
decodeOp comp =
  case ma addr of
    99 -> Halt
    1  -> Add       { v1addr = ma $ addr + 1, v2addr = ma $ addr + 2, resultAddr = ma $ addr + 3 }
    2  -> Multiply  { v1addr = ma $ addr + 1, v2addr = ma $ addr + 2, resultAddr = ma $ addr + 3 }
    v  -> Other v
  where
    addr = _curAddr comp
    ma = getMemAt comp
    _ = trace ("decodeOp addr" ++ show addr ) ()

setMemAt :: Address -> MemValue -> State Computer ()
setMemAt addr v = do
  comp <- get
  let mem = _memory comp
  put $ comp { _memory = insert addr v mem }

advance :: Int -> State Computer ()
advance places = do
  comp <- get
  put $ comp { _curAddr = _curAddr comp + places }

execInstruction :: Instruction -> State Computer ()
execInstruction ins = do
  comp <- get
  case ins of
    Halt -> put $ comp { _runState = Halted }
    Add a1 a2 ar -> 
      let v1 = getMemAt comp a1 in
      let v2 = getMemAt comp a2 in
      setMemAt ar (v1 + v2)
      >> advance 4
    Multiply a1 a2 ar ->
      let v1 = getMemAt comp a1 in 
      let v2 = getMemAt comp a2 in
      setMemAt ar (v1 * v2)
      >> advance 4
    Other v -> put $ comp { _runState = Fault ("Unknown instruction, value: " ++ show v) }

loadAndRunFile :: FilePath -> IO Int
loadAndRunFile memFileName = do
  raw <- fileOfCSVToIntList memFileName
  return $ loadAndRun (Just (12,2)) raw
  
compFromRaw :: [Int] -> Computer
compFromRaw raw =
  let xs = zip ([0..] :: [Int]) raw in
  Computer 
    { _memory = Data.IntMap.Strict.fromList xs
    , _runState = Running
    , _curAddr = 0 }

loadAndRun :: Maybe (Int,Int) -> [Int] -> Int
loadAndRun preset raw =
  let initState = compFromRaw raw in
  let comp = evalState (execComputer preset) initState in
  getMemAt comp 0

execComputer :: Maybe (Int,Int) -> State Computer Computer
execComputer 
  (Just (noun,verb)) = do
    setMemAt 1 noun
    >> setMemAt 2 verb
    >> execComputer Nothing
execComputer 
  Nothing = do
    comp <- get 
    case _runState comp of
      Running -> 
        execInstruction (decodeOp comp)
        >> execComputer Nothing
      Halted -> return comp
      InputWait -> return comp
      Fault reason -> trace ("execComputer - Fault: " ++ reason) (return comp)

test =
  let xs = [1,9,10,3,2,3,11,0,99,30,40,50]  in
  loadAndRun Nothing xs

solvePart1 =
  loadAndRunFile "./data/Day02.txt"

solvePart2 = do
  raw <- fileOfCSVToIntList "./data/Day02.txt"
  -- assuming the solution will be closer to the middle value than the extremes, sort by
  -- the net distance from the midpoint tuple. Roughly 87% savings in cases, 85% savings
  -- on time, and 86% savings on memory
  let cases = 
        sortBy (\(n,v) (n',v') -> compare (abs (n-50) + abs (v-50)) (abs (n'-50) + abs (v'-50))) 
        [(noun,verb) | noun <- [0..99], verb <- [0..99]]
  where 
    findWinner :: [(Int,Int)] -> [Int] -> (Int,Int,Int)
    findWinner [] _ = (0,0,0)
    findWinner ((n,v):xs) raw =
      case loadAndRun (Just (n,v)) raw of
        19690720 -> (n,v, 100*n+v)
        _ -> findWinner xs raw