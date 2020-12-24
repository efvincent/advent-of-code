module Y20.Day23 where

import qualified  Data.CircularList as CL

t = "583976241"

parseRaw :: [Char] -> CL.CList Int
parseRaw = CL.fromList . map (read . (:[])) 