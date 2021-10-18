module Y20.Day18p2 (solve20d18p2) where

import Parsing (parse,  Alternative((<|>)), Parser, integer, symbol ) 

{-
Grammer for the "correct" simple arithmetic with parenthesis

expr ::= term + expr | term         Lowest level of priority
term ::= factor * term | factor     Middle level of priority
factor ::= (expr) | int             Highest level of priority
-}

-- expr and term operators are switched per AoC puzzle

expr :: Parsing.Parser Int
expr = do 
  x <- term; symbol "*"; y <- expr ; pure (x*y) 
  <|> term

term :: Parsing.Parser Int
term = do
  x <- factor; symbol "+"; y <- term; pure (x+y)
  <|> factor

factor :: Parsing.Parser Int
factor = do 
  symbol "("; x <- expr; symbol ")"; pure x
  <|> integer

solve20d18p2 :: IO ()
solve20d18p2 = do
  raw <- readFile "./data/20/Day18.txt"
  let xs = map fst $ concatMap (parse expr) $ lines raw
  print $ sum xs