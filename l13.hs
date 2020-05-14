aList :: [[Char]]
aList = ["cat","dog","mouse"]

data Icecream = Vanilla | Chocolate deriving (Show, Eq, Ord)

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n 