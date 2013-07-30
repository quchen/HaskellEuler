{-
      Problem 19
            How many Sundays fell on the first of the month during the twentieth
            century (1 Jan 1901 to 31 Dec 2000)?

      Result
            171
            .06 s

      Comment
            This solution uses no built-in date functions, and relies only on
            basic Prelude functions. There's some added stuff such as Show
            instances that were used for debugging.

-}

module Problem19 (solution) where

import Text.Printf
import CommonFunctions


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

add7Days :: Date -> Date
add7Days date@(Date y m d) = Date y' m' d'
      where dim = daysInMonth date
            flipMonth = (d+7) > dim
            flipYear  = flipMonth && m == 12
            d' = constrain 1 dim (d+7)
            m' = if flipMonth then constrain 1 12 (m+1) else m
            y' = if flipYear then y+1 else y

-- | Constrains values to be in [start,end] by modulo operation.
--   Example: constrain 1 10 11 = 1, as 11 corresponds to 1 in the interval from
--   1 to 10.
constrain start end value | start > end = constrain end start value
                          | otherwise = (value - start) `rem` (end - start + 1) + start

isFirst (Date _ _ 1) = True
isFirst _            = False

solution = length' . filter isFirst $ centuryDays

-- | All the days of the century in question
centuryDays = takeWhile (<= Date 2000 12 31) .
              dropWhile (<  Date 1901  1  1) $
              iterate add7Days (Date 1900 1 7) -- 7th of Jan was the first Sunday