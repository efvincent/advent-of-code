module Y18.Day04 where

  -- [1518-11-22 00:00] Guard #1231 begins shift
  -- [1518-04-13 00:00] falls asleep

import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.String

s = "[1518-11-22 00:00] Guard #1231 begins shift"

rdate = "[0-9]{2}-[0-9]{2}"
rtime = "[0-9]{2}:[0-9]{2}"

rguard = "#([0-9]+)"