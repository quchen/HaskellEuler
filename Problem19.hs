{-# LANGUAGE BangPatterns #-}

{-
      Problem 19
            How many Sundays fell on the first of the month during the twentieth
            century (1 Jan 1901 to 31 Dec 2000)?

      Result
            171
            .34 s

      Comment
            This solution uses no built-in date functions, and relies only on
            basic Prelude functions. There's some added stuff such as Show
            instances that were used for debugging.

-}

module Problem19 (solution) where

import Text.Printf
import Data.List (genericLength)

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Show, Enum, Eq)

addDayToWeekday :: Weekday -> Weekday
addDayToWeekday Sunday = Monday -- Sunday needs special handling to loop properly
addDayToWeekday d      = succ d



data Date = Date !Int !Int !Int -- Year Month Day
      deriving (Eq, Ord)
instance Show Date where
      show (Date y m d) = printf "%d-%02d-%02d" y m d

daysInMonth (Date y m _) | m `elem` [1,3,5,7,8,10,12] = 31
                         | m `elem` [4,6,9,11]        = 30
                         | isLeapYear y               = 29
                         | otherwise                  = 28

isLastDayOfMonth date@(Date _ _ d) = d == daysInMonth date

implies a b = not a || b

isLeapYear y = y `rem` 4 == 0
               &&
               ((y `rem` 100 == 0) `implies` (y `rem` 400 == 0))

addDayToDate :: Date -> Date
addDayToDate date@(Date y m d) | isLastDayOfMonth date && m == 12 = Date (y+1) 1 1 -- Flip year
                               | isLastDayOfMonth date            = Date y (m+1) 1 -- Flip month
                               | otherwise                        = Date y m (d+1)

data WeekdayDate = WD Date Weekday
instance Show WeekdayDate where
      show (WD d w) = printf "%s (%s)" (show d) (show w)

addDay :: WeekdayDate -> WeekdayDate
addDay (WD d w) = WD (addDayToDate d) (addDayToWeekday w)

isFirstSunday (WD (Date _ _ 1) Sunday) = True
isFirstSunday _                        = False

solution = Just . genericLength . filter isFirstSunday $ centuryDays

centuryDays = takeWhile (\ ~(WD d _) -> d <= Date 2000 12 31) .
              dropWhile (\ ~(WD d _) -> d <  Date 1901  1  1) $
              iterate addDay (WD (Date 1900 1 1) Monday)