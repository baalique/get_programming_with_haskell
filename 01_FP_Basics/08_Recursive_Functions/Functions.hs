customDrop _ [] = []
customDrop 0 lst = lst
customDrop n (x : xs) = customDrop (n - 1) xs

customLength [] = 0
customLength (_ : xs) = 1 + customLength xs

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz = collatz' []

collatz' xs 1 = xs ++ [1]
collatz' xs n = collatz' (xs ++ [n]) m
  where
    m =
      if even n
        then div n 2
        else n * 3 + 1
