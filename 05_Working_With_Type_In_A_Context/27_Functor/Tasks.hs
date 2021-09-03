newtype Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box x) = Box (f x)

morePresents :: Box a -> Int -> Box [a]
morePresents box n = replicate n <$> box

aBox :: Box Int
aBox = Box 1

wrapped :: Box (Box Int)
wrapped = fmap Box aBox

unwrap :: Box a -> a
unwrap (Box a) = a
