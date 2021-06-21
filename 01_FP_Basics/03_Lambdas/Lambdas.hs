sumSquareOrSquareSum x y = max sumSquare squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (^ 2) (x + y)

sumSquareOrSquareSum' x y = (\sumSquare squareSum -> max sumSquare squareSum) (x ^ 2 + y ^ 2) ((x + y) ^ 2)

doubleDouble x = (\dubs -> 2 * dubs) (x * 2)

overwrite x = let x = x 
              in let x = 3
                  in let x = 4
                     in x

overwrite' x = (\x -> (\x -> (\x -> x) 4) 3) 2

counter x = (\x -> (\x -> x + 1) x + 1) x
