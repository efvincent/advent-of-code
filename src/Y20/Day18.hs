module Y20.Day18 (solve20d18p1) where

import Debug.Trace
import Data.Char (digitToInt)

data Expr = Add Expr Expr 
          | Mul Expr Expr
          | Paren Expr
          | Val Int 
          | Void
          deriving Show

data Token  = Num Int
            | Parens [Token]
            | Times | Plus 
            deriving (Show, Eq)

t1 :: [Char]
t1 = "2 * 3 + (4 * 5)"
t2 :: [Char]
t2 = "5 + (8 * 3 + 9 + 3 * 4 * 3)"
t3 :: [Char]
t3 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
t4 :: [Char]
t4 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

tokenize :: String -> [Token]
tokenize s = 
  let (ts, _) = go [] s in ts
  where
    go :: [Token] -> [Char] -> ([Token], [Char])
    go acc [] = (acc, [])
    go acc (x:xs) 
      | x == ' '  = go acc xs
      | x == '('  = 
        let (inside, xs') = go [] xs in 
        go (acc ++ [Parens inside]) xs'  
      | x == ')'  = (acc, xs)
      | x == '+'  = go (acc ++ [Plus]) xs
      | x == '*'  = go (acc ++ [Times]) xs
      | otherwise = go (acc ++ [Num (digitToInt x)]) xs

parseOp :: Expr -> [Token] -> (Expr, [Token])
parseOp e [] = (e, [])
parseOp e (Plus  : Num n : ts) = parseOp (Add e (Val n)) ts
parseOp e (Times : Num n : ts) = parseOp (Mul e (Val n)) ts
parseOp e (Plus  : Parens inner : ts) = 
  let (b,ts') = parse inner in parseOp (Add e (Paren b)) ts
parseOp e (Times : Parens inner : ts) = 
  let (b,ts') = parse inner in parseOp (Mul e (Paren b)) ts

parse :: [Token] -> (Expr, [Token])
parse [] = (Void, [])
parse [Num a] = (Val a, [])
parse (Num a : op : ts) = parseOp (Val a) (op:ts)
parse (Parens inner: op : ts) = 
  let (b,ts') = parse inner in 
  parseOp (Paren b) $ op:ts
parse [Parens inner] = let (b,ts') = parse inner in (Paren b, ts')

eval :: Expr -> Int
eval (Val n) = n
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b
eval (Paren e) = eval e

pexpr :: Expr -> String
pexpr (Val n) = show n
pexpr (Mul a b) = pexpr a ++ " * " ++ pexpr b
pexpr (Add a b) = pexpr a ++ " + " ++ pexpr b
pexpr (Paren e) = "(" ++ pexpr e ++ ")"

calc :: String -> Int
calc = eval . fst . parse . tokenize
pcalc :: String -> String
pcalc = pexpr . fst . parse .tokenize

solve20d18p1 :: IO ()
solve20d18p1 = do
  raw <- readFile "./data/20/Day18.txt"
  let xs = map calc $ lines raw
  print $ sum xs