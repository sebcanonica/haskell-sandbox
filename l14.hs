--data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Ord, Eq)
instance Show SixSidedDie where
   show S1 = "I"
   show S2 = "II"
   show S3 = "III"
   show S4 = "IV"
   show S5 = "V"
   show S6 = "VI"

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving Enum

instance Eq SixSidedDie where
    (==) dA dB = fromEnum dA == fromEnum dB

instance Ord SixSidedDie where
    compare dA dB = fromEnum dA `compare` fromEnum dB

data FiveSidedDie = F1 | F2 | F3 | F4 | F5

class PhysicalObject d => Die d where
    roll :: d -> Int

class PhysicalObject o where
    describe :: o -> String

instance Die FiveSidedDie where
    roll _ = 1

instance PhysicalObject FiveSidedDie where
    describe _ = "A five sided die"

data FourSidedDie = Side1 | Side2 | Side3 | Side4 deriving (Enum, Eq, Show)

class (Eq a, Enum a) => Die2 a where -- not a method of the object but just the result type
  roll2 :: Int -> a

instance Die2 FourSidedDie where
  roll2 n = toEnum (n `mod` 4)

rolledDie = roll2 1 :: FourSidedDie