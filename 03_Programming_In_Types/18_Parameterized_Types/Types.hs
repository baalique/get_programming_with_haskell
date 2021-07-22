newtype Box a = Box a deriving Show

wrap :: a -> Box a
wrap = Box

unwrap :: Box a -> a
unwrap (Box x) = x

data Triple a = Triple a a a
    deriving Show

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.1 0.2 0.3

type FullName = Triple String

aFullName :: FullName
aFullName = Triple "Howard" "Philips" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'P' 'L'

first :: Triple a -> a
first (Triple f _ _) = f

second :: Triple a -> a
second (Triple _ s _) = s

third :: Triple a -> a
third (Triple _ _ t) = t

toList :: Triple a -> [a]
toList (Triple x y z) = [x, y, z]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple x y z) = Triple (f x) (f y) (f z)

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box x) = Box (f x)
