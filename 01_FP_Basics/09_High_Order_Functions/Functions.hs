map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' _ [] = []
filter' f (x : xs) = if f x then x : rest else rest
  where
    rest = filter' f xs

remove' _ [] = []
remove' f (x : xs) = if f x then rest else x : rest
  where
    rest = remove' f xs

foldl'' _ init [] = init
foldl'' f init (x : xs) = foldl'' f (f init x) xs

product' xs = foldl (*) 1 xs
