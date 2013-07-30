{-# LANGUAGE BangPatterns #-}

{-
      Problem 73
            How many reduced fractions are between 1/3 and 1/2 with maximum
            denominator 12000?

      Result
            7295372
            108 s

      Comment
            1. See problem 71 for more details on this, namely Farey Sequences.
            2. Using tuples (numerator,denominator) makes this 3 times faster
               compared to using Data.Ratio. (Maybe because my mediant is
               strict?)
-}
module Problem73 (solution) where


solution = fromIntegral $ fareyCount 12000 (1,3) (1,2)

mediant (!a,!b) (!c,!d) = (ac', bd')
      where ac   = a + c
            bd   = b + d
            gcd' = gcd ac bd
            ac'  = ac `quot` gcd'
            bd'  = bd `quot` gcd'

-- n: max denominator, ab/cd: fractions to search between
fareyCount :: Int       -- ^ Max denominator
           -> (Int, Int) -- ^ Lower boundary
           -> (Int, Int) -- ^ Upper boundary
           -> Int
fareyCount n ab cd
      | d > n = 0
      | otherwise = 1 + fareyCount n ab m + fareyCount n m cd
      where m@(_,d) = mediant ab cd -- Mediant/denominator