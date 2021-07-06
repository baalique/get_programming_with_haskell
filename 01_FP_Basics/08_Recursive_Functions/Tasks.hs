customReverse [] = []
customReverse (x:xs) = customReverse xs ++ [x]

fastFib = fastFib' 1 1

fastFib' _ _ 0 = 0
fastFib' _ _ 1 = 1
fastFib' _ _ 2 = 1
fastFib' x y 3 = x + y
fastFib' x y counter = fastFib' (x + y) x (counter - 1)
