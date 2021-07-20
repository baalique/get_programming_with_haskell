data AuthorName = AuthorName
    { firstName :: String
    , lastName  :: String
    }

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name                FirstName LastName
          | NameWithMiddle      FirstName MiddleName LastName
          | TwoInitialsWithLast Char      Char       LastName
          | FirstNameWithInit   FirstName Char
    deriving Show

data Creator = AuthorCreator Author | ArtistCreator Artist
    deriving Show

data Author = Author Name
    deriving Show

data Artist = Person Name | Band String deriving Show

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

data Book = Book
    { author    :: Creator
    , isbn      :: String
    , bookTitle :: String
    , bookYear  :: Int
    , bookPrice :: Double
    }

data VinylRecord = VinylRecord
    { artist      :: Creator
    , recordTitle :: String
    , recordYear  :: Int
    , recordPrice :: Double
    }

data CollectibleToy = CollectibleToy
    { toyName        :: String
    , toyDescription :: String
    , toyPrice       :: Double
    }

data Pamphlet = Pamphlet
    { pamphletName        :: String
    , pamphletDescription :: String
    , contacts            :: String
    }

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem     book  ) = bookPrice book
price (RecordItem   record) = recordPrice record
price (ToyItem      toy   ) = toyPrice toy
price (PamphletItem _     ) = 0

madeBy :: StoreItem -> String
madeBy (BookItem   book  ) = show $ author book
madeBy (RecordItem record) = show $ artist record
madeBy _                   = "unknown"
