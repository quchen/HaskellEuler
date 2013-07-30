{-
      Problem 30
            Numbers that can be written as powers of their digits

      Result
            443839
            6.28 s

      Comment
            The upper boundary can be estimated since 999... = 10^k - 1 has to
            be equal to 9^5 + 9^5 + ... = k 9^5, which yields the maximum
            condition k 9^5 = 10^k - 1. A numeric solution for this is 5.51257,
            which yields a maximum of 10^5.51257 = 325514.24.
-}
module Problem30 (solution) where

import CommonFunctions


solution = fromIntegral . sum' $ filter is5thPowerSum [2..325515]

-- Can x be written as the sum of fifth power of its digits?
is5thPowerSum x = x == (sum' . map toTheFifth $ show x)

-- Memoize powers
toTheFifth '0' = 0^5
toTheFifth '1' = 1^5
toTheFifth '2' = 2^5
toTheFifth '3' = 3^5
toTheFifth '4' = 4^5
toTheFifth '5' = 5^5
toTheFifth '6' = 6^5
toTheFifth '7' = 7^5
toTheFifth '8' = 8^5
toTheFifth '9' = 9^5
toTheFifth  _  = error "Not a digit 'to the fifth'  in problem 30"