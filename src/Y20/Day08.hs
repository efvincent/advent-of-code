module Y20.Day08 (solve20d8p1,solve20d8p2,fn20d8) where

import Data.IntMap.Strict ((!), fromList, toList, IntMap, insert)

fn20d8 :: [Char]
fn20d8 = "./data/20/Day08.txt"

data OpCode = ACC | JMP | NOP deriving (Show, Eq)
type Arg = Int
type Count = Int
type Address = Int
type Instruction = (OpCode, Arg, Count)
type Program = IntMap Instruction
data Computer = Computer {
    _addr :: Address,
    _term :: Address,
    _mem :: Program,
    _acc :: Int
  } deriving (Show)

-- | Makes a computer from the raw string input from the puzzle
mkComputer :: String -> Computer 
mkComputer s = 
  let ins = mkInstructions s in
  Computer { _addr = 0, _term = length ins, _mem = ins, _acc = 0 }
  where
    mkInstructions :: String -> Program
    mkInstructions s = fromList $ zip [0..] $ map (addRunCount . mkIns) $ lines s
      where
        addRunCount :: (OpCode, Arg) -> (OpCode, Arg, Count)
        addRunCount (op,arg) = (op,arg,0)

        mkIns :: String -> (OpCode, Int)
        mkIns s =
          let [op, amt] = words s in
          (readOp op, readAmt amt)
          where
            readOp :: String -> OpCode
            readOp "acc" = ACC
            readOp "jmp" = JMP
            readOp "nop" = NOP

            readAmt :: String -> Int
            readAmt (s:ss) | s == '+' = read ss
            readAmt s = read s

-- | Runs a computer returning either `Left String` where the string is the error, or
-- `Right Int` where the int is the value of the accumulator after the run
runComputer :: Computer -> Either String Int
runComputer comp =
  let addr = _addr comp in
  let term = _term comp in
  if addr >= term then Right $ _acc comp else
  case _mem comp ! addr of      
    (_, _, _)     | addr == term -> Right $ _acc comp
    (_, _, count) | count > 0 -> Left $ "multi instruction execute at " ++ show addr ++ ". acc = " ++ show (_acc comp)
    (JMP,   0, _) -> Left "Infinite Loop - JMP 0" 
    (ACC, arg, _) -> runComputer $ deltaAddr 1   . incOpCount addr . adjAcc arg $ comp
    (JMP, arg, _) -> runComputer $ deltaAddr arg . incOpCount addr $ comp
    (NOP, _  , _) -> runComputer $ deltaAddr 1   . incOpCount addr $ comp
  where
    incOpCount :: Address -> Computer -> Computer
    incOpCount addr comp =
      comp { _mem = insert addr (op, arg, count + 1) mem }
      where
        mem = _mem comp
        (op, arg, count) = mem ! addr

    adjAcc :: Int -> Computer -> Computer
    adjAcc v comp = comp { _acc = _acc comp + v }

    deltaAddr :: Int -> Computer -> Computer
    deltaAddr v comp = comp { _addr = _addr comp + v }

-- | Fixes the infinite loop by building a list of alternate operations (where JMP has been changed
-- to NOP for all JMP, appended to where NOPs have been change to JMP for all NOP). It then tries
-- each of these alternate operations until the computer runs without error. Note that b/c Haskell
-- is lazy, it won't even generate all the alternate options until they come up in the loop.
fixInfLoop :: Computer -> Either String Int
fixInfLoop comp =
  loop alt
  where
    part1 = map (\(addr, (_, arg, c)) -> (addr,(NOP, arg, c))) 
            $ filter (\(_, (op, _, _)) -> op == JMP) $ toList $ _mem comp
    alt = part1 ++ 
            map (\(addr, (_, arg, c)) -> (addr,(JMP, arg, c))) 
                (filter (\(_, (op, _, _)) -> op == NOP) $ toList $ _mem comp)
    loop [] = Left "No Solution found"
    loop ((k,v):ms) = 
      case runComputer (comp { _mem = insert k v (_mem comp)}) of
        Right n -> Right n
        Left _ -> loop ms

solve20d8p1 :: String -> IO ()
solve20d8p1 fn = do
  raw <- readFile fn
  print $ runComputer $ mkComputer raw

solve20d8p2 :: String -> IO ()
solve20d8p2 fn = do
  raw <- readFile fn
  print $ fixInfLoop $ mkComputer raw
