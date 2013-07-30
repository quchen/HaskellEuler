{-
      Problem 17
            How many letters are used when counting from 1 to 1000 in (British)
            english?

      Result
            21224
            .006 s
-}
module Problem17 (solution) where

import Data.Char (isLetter)
import Data.List
import CommonFunctions



solution = sum' $ map numberLength [1..1000]

numberLength = length' . filter isLetter . spellNumber

spellNumber n
      | n ==   0  = ""
      | n ==   1  = "one"
      | n ==   2  = "two"
      | n ==   3  = "three"
      | n ==   4  = "four"
      | n ==   5  = "five"
      | n ==   6  = "six"
      | n ==   7  = "seven"
      | n ==   8  = "eight"
      | n ==   9  = "nine"
      | n ==  10  = "ten"
      | n ==  11  = "eleven"
      | n ==  12  = "twelve"
      | n ==  13  = "thirteen"
      | n ==  14  = "fourteen"
      | n ==  15  = "fifteen"
      | n ==  16  = "sixteen"
      | n ==  17  = "seventeen"
      | n ==  18  = "eighteen"
      | n ==  19  = "nineteen"
      | n <=  29  = "twenty"  ++ spellNumber (n `rem` 10) -- There's a space
      | n <=  39  = "thirty"  ++ spellNumber (n `rem` 10) --  missing, e.g.
      | n <=  59  = "fifty"   ++ spellNumber (n `rem` 10) --  "twentyone", but
      | n <=  69  = "sixty"   ++ spellNumber (n `rem` 10) --  since the result
      | n <=  79  = "seventy" ++ spellNumber (n `rem` 10) --  is independent of
      | n <=  89  = "eighty"  ++ spellNumber (n `rem` 10) --  this, I haven't
      | n <=  99  = "ninety"  ++ spellNumber (n `rem` 10) --  adressed the issue
      | n <= 999  = let (q,r) = n `quotRem` 100
                        andWord = if r /= 0 then " and " else ""
                    in  spellNumber q ++ " hundred" ++ andWord ++ spellNumber r
      | otherwise = "one thousand"