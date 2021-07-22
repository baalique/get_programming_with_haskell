data List a = Empty | Cons a (List a) deriving Show

builtinEx1 :: [Int]
builtinEx1 = 1 : 2 : 3 : []

customEx1 :: List Int
customEx1 = Cons 1 (Cons 2 (Cons 3 Empty))

builtinEx2 :: [Char]
builtinEx2 = 'a' : 'b' : 'c' : []

customEx2 :: List Char
customEx2 = Cons 'a' (Cons 'b' (Cons 'c' Empty))

customMap :: (a -> b) -> List a -> List b
customMap _ Empty       = Empty
customMap f (Cons x xs) = Cons (f x) (customMap f xs)
