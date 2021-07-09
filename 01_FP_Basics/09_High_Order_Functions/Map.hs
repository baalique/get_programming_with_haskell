map' _ [] = []
map' f (x : xs) = f x : map' f xs

mapTail = mapTail' []

mapTail' acc _ [] = acc
mapTail' acc f (x : xs) = mapTail' (acc ++ [f x]) f xs

mapTailReverse f xs = reverse $ mapTailReverse' [] f xs

mapTailReverse' acc _ [] = acc
mapTailReverse' acc f (x : xs) = mapTailReverse' (f x : acc) f xs
