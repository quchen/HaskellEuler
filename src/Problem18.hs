{-
      Problem 18
            Maximum sum in a triangle of numbers

      Solution idea
            Reduce the triangle row by row, starting from the bottom. For
            example, the bottom right corner is the triangle [[31],[04,23]]. If
            one reaches the 31, the maximizing step is taking 23, 04 is not an
            option. Therefore, the cell with 31 can effectively be considered a
            cell on the bottom of the triangle with value 31+23=54. Doing this
            for all 3-digit triangles in the bottom row reduces the triangle by
            one row; iterating the process until the tip of the pyramid has been
            reached yields the desired result.

      Result
            1074
            .01 s
-}
module Problem18 (
      solution,
      reduceTriangle -- Exported because problem 67 uses the same algorithm
) where



solution = reduceTriangle triangle

reduceTriangle = head . foldr1 rowReduce

-- rowReduce compares all the small triangles in the bottom rows,
-- and reduces the two lines to one effective line. Folding this
-- over the pyramid yields a singleton list containing the result.
-- Example: rowReduce [1,2] [7,1,4] -> [8,6]
rowReduce upper lower = zipWith max lShifted rShifted
      where lShifted = zipWith (+) upper (init lower) -- init unnecessary, but makes the code nicely symmetric :-)
            rShifted = zipWith (+) upper (tail lower)
-- -- Golfed version:
-- rowReduce u l=let p f=z(+)u$f l;z=zipWith in z max(p id)$p tail





triangle = [
      [75],
      [95, 64],
      [17, 47, 82],
      [18, 35, 87, 10],
      [20, 04, 82, 47, 65],
      [19, 01, 23, 75, 03, 34],
      [88, 02, 77, 73, 07, 63, 67],
      [99, 65, 04, 28, 06, 16, 70, 92],
      [41, 41, 26, 56, 83, 40, 80, 70, 33],
      [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
      [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
      [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
      [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
      [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
      [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]
      ]