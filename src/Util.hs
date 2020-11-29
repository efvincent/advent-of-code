module Util where

-- | Runs a single test, by checking 'control' is equal to 'candidate'
-- Returns 'Either' where 'Right ()' is success, and 'Left msg' is the message supplied
-- in a failure case
check :: Eq a => String -> a -> a -> Either String ()
check msg control candidate =
  if control == candidate then Right () else Left msg

-- | Given a filename that's assumed to contain a list of what can be read as
-- 'Int', with one 'Int' per line, returns 'IO [Int]' which is all the numbers
-- in the file in the order they appeared in the file
fileToIntList :: FilePath -> IO [Int]
fileToIntList filename = do
  raw <- readFile filename
  return $ map read $ words raw