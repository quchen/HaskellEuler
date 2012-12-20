{-
      Problem 4
            A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 ? 99.
            Find the largest palindrome made from the product of two 3-digit numbers.

      Result
            .67 s
-}
module Problem4 where

import CommonFunctions (maximum')

solution = maximum' [i*j | i <- [1..999], j <- [1..i], isPalindromic  (i*j)]
      where isPalindromic n = let showNumber = show n
                              in  showNumber == reverse showNumber