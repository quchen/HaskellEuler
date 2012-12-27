{-
      Problem 34
            Numbers that can be written as factorials of their digits

      Result
            40730
            13 s

      Comment
            Similar estimate as in problem 30 results in a numerical upper
            boundary estimate of 2309192.
-}
module Problem34 (solution) where

import CommonFunctions
import Control.Applicative

solution = fromIntegral . sum' $ filter isSumOfFacDigits [3..2309192]

-- f x = can x be written as the sum of faculties of its digits?
isSumOfFacDigits = (==) <*> sum' . map faculty' . show

-- This is even (much) faster than using a vector lookup table :-)
faculty' '0' = 1
faculty' '1' = 1
faculty' '2' = 2
faculty' '3' = 6
faculty' '4' = 24
faculty' '5' = 120
faculty' '6' = 720
faculty' '7' = 5040
faculty' '8' = 40320
faculty' '9' = 362880