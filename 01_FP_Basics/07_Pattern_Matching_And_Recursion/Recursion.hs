customTake _ [] = []
customTake 0 _ = []
customTake n (x : xs) = x : customTake (n - 1) xs

customGcd a b = if m == 0 then b else customGcd b m
  where
    m = mod a b

customTail [] = []
customTail (__ : xs) = xs

customGcd' a 0 = a
customGcd' a b = customGcd' b (a `mod` b)
