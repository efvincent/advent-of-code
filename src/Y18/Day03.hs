module Y18.Day03 where

data Point = Point { _x ::Int, _y :: Int }

data Patch = 
  Patch { _id :: String
        , _ul :: Point
        , _lr :: Point }

