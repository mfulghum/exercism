module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time.Calendar (Day, gregorianMonthLength, fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Char

data Schedule = Teenth | First | Second | Third | Fourth | Last
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Enum)

getDayRange :: Integer -> Int -> Schedule -> [Int]
getDayRange _ _ Teenth = [13..19]::[Int]
getDayRange _ _ First = [1..7]::[Int]
getDayRange _ _ Second = [8..14]::[Int]
getDayRange _ _ Third = [15..21]::[Int]
getDayRange _ _ Fourth = [22..28]::[Int]
getDayRange year month Last =
  let monthLength = gregorianMonthLength year month
  in [monthLength,monthLength-1..monthLength-6]

dayToWeekday :: Day -> Int
dayToWeekday = flip (-) 1 . digitToInt . head . (formatTime defaultTimeLocale "%u")

meetupDay :: Schedule -> Weekday -> Integer -> Int -> Day
meetupDay schedule weekday year month =
  head [day | day <- map (fromGregorian year month) (getDayRange year month schedule), fromEnum weekday == dayToWeekday day]
