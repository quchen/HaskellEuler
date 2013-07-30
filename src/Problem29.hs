{-
      Problem 29
            How many distinct values of a^b for 2 <= a,b <= 100 are there?

      Result
            9183
            .94 s
-}
module Problem29 (solution) where

import Control.Applicative
import Data.List
import CommonFunctions (length')

solution = numDistinct powers
      where numDistinct = length' . group . sort
            powers = liftA2 (^) [2..100] [2..100]