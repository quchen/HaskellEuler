{-# LANGUAGE BangPatterns #-}

{-
      Problem 71
            Find the numerator of the fraction before 3/7 in the Farey sequence
            for d <= 10^6.

      Result
            428570
            1.05 s

      Comment
            This algorithm was constructed using the small note from [1], which
            reads "For a method of computing a successive sequence from an
            existing one of n terms, insert the mediant fraction (a+b)/(c+d)
            between terms a/c and b/d when c+d<=n".

            This is *very* brief, so here's an elaboration on that:

            The mediant of two fractions a/b and c/d is (a+b)/(c+d). This
            fraction has two properties that are important here:
                  1. a/b < mediant < c/d
                  2. The mediant's denominator is the smallest denominator of
                     all the (reduced) fractions that lie between a/b and c/d.
            We're now looking for the fraction before 3/7 in the Farey Sequence.
            Calculating the mediant of these two has two possible outcomes:
                  x) The mediant has a denominator greater than n, the index
                     of the Farey sequence. Since we know from 1. that it's
                     between the two fractions we put in and from 2. that it's
                     the one with the smallest possible denominator, we can
                     conclude that this mediant is not part of the Farey
                     Sequence. Therefore, the two input fractions are adjacent.
                  y) The mediant is in the Farey Sequence and the denominator
                     is not too large. From 1. we can be sure that it's
                     strictly between the two input fractions. If we now change
                     the first input fraction to the generated mediant and
                     recurse, the algorithm is called again with a narrower
                     focus. Repeating this will eventually yield case x), and
                     the algorithm terminates.
            So to calculate the fraction before 3/7, the algorithm moves
            the left boundary towards 3/7, until it detects that there's no
            fraction between the boundaries, in which case two neighbours have
            been found.

            A final note: using self-made strict tuples makes this a little bit
            faster than using Data.Ratio for the fractions.
-}
module Problem71 (solution) where

solution = fst $ fareyBefore (10^6) (1,7) (3,7) -- TODO: should 0 be (0,1) instead? 0::Ratio Int == 0%1

fareyBefore n ac@(!a,!c) bd@(!b,!d)
      | c + d > n = ac
      | otherwise = fareyBefore n abcd bd
      where
            ab = a + b
            cd = c + d
            gcd' = gcd ab cd
            abcd = (ab `quot` gcd', cd `quot` gcd')

{-
Here's an algorithm to calculate the full Farey sequence F_n, deduced from
http://en.wikipedia.org/wiki/Farey_sequence#Next_term

fareySequence :: (Integral a) => a -> [Ratio a]
fareySequence n = 0 : unfoldr nextFarey (0 % 1, 1 % n)
      where
            -- Computes the next fraction out of the predecessors
            -- a%b and c%d.
            -- Reference: http://en.wikipedia.org/wiki/Farey_sequence#Next_term
            nextFarey (ab, cd)
                  | ab == 1 = Nothing
                  | otherwise = Just (cd, (cd, pq))
                  where
                        (a,b) = (numerator ab, denominator ab)
                        (c,d) = (numerator cd, denominator cd)
                        pq = p % q
                        k = (n + b) `div` d
                        p = k * c - a
                        q = k * d - b
-}