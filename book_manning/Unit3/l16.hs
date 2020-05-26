data AuthorName = AuthorName {
    firstName :: String,
    lastName :: String
}

data Car = Car
data Spoiler = Spoiler
data SportsCar = SportsCar Car Spoiler

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName | NameWithMiddle FirstName MiddleName LastName deriving Show

data Creator = AuthorCreator Author | ArtistCreator Artist deriving Show
data Author = Author Name deriving Show
data Artist = Person Name | Band String deriving Show

data Book = Book {
     author    :: Creator
   , isbn      :: String
   , bookTitle :: String
   , bookYear  :: Int
   , bookPrice :: Double
   }

data VinylRecord = VinylRecord {
     artist        :: Creator
   , recordTitle   :: String
   , recordYear    :: Int
   , recordPrice   :: Double
   }

data Pamphlet = Pamphlet {
     title :: String
   , description :: String
   , contact :: String
}

data StoreItem = BookItem Book 
               | RecordItem VinylRecord
               | PamphletItem Pamphlet

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price _ = 0

ericEvans = AuthorCreator (Author (Name "Eric" "Evans"))
ddd = Book ericEvans "111" "Domain Driven Design" 2002 45.5
madeByOutput = madeBy (BookItem ddd)

shortStory = Pamphlet "Little Red Hood" "Classic" "guntenberg"
dddPrice = price (BookItem ddd)
redHoodPrice = price (PamphletItem shortStory)

data Shape = Circle  { radius :: Double } 
           | Square {sizeLength :: Double } 
           | Rectangle { size1Length :: Double, size2Length :: Double }

perimeter :: Shape -> Double
perimeter (Circle radius) = 2*pi*radius
perimeter (Square sizeLength) = 4*sizeLength
perimeter (Rectangle size1Length size2Length) = 2*(size1Length+size2Length)

aCircle = Circle 12
aSquare = Square 10
aRectangle = Rectangle 5 2

