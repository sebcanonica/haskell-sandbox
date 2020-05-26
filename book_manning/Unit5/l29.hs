longString :: Maybe String
longString = (++) <$> Just "a" <*> Just "b"

ioHello :: IO String
ioHello = pure "Hello World"


doorPrize :: [Int]
doorPrize = [1000,2000,3000]

boxPrizeMultiplier :: [Int]
boxPrizeMultiplier = [2,5]

totalPrize :: [Int]
totalPrize = pure (*) <*> doorPrize <*> boxPrizeMultiplier

data User = User {
     name :: String
   , gamerId :: Int
   , score :: Int
   } deriving Show

testNames :: [String]
testNames = ["John Smith"
            ,"Robert'); DROP TABLE Students;--"
            ,"Christina NULL"
            ,"Randall Munroe"
            ,"SCA"]

testIds :: [Int]
testIds = [1337
          ,0123
          ,999999]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]

testData :: [User]
testData = pure User <*> testNames
                       <*> testIds
                       <*> testScores


allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func v = (pure func) <*> v

test1 = allFmap (+ 1) [1,2,3]
test2 = allFmap (+ 1) (Just 5)
test3 = allFmap (+ 1) Nothing


example :: Int
example = (*) ((+) 2 4) 6

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> pure ((+) 2 4) <*> pure 6

packSize = [6, 12] :: [Int]
consumed = [4] :: [Int]
friends = [2, 3] :: [Int]
need = [3, 4] :: [Int]

outcomes :: [Int]
outcomes = pure (-) <*> needed <*> inStock
    where needed = pure (*) <*> totalPeople <*> need
          totalPeople = (+2) <$> friends
          inStock = pure (-) <*> packSize <*> consumed


startingBeer :: [Int]
startingBeer = [6,12]

remainingBeer :: [Int]
remainingBeer = (\count -> count - 4) <$> startingBeer

guests :: [Int]
guests = [2,3]

totalPeople :: [Int]
totalPeople = (+ 2) <$> guests

beersPerGuest :: [Int]
beersPerGuest = [3,4]

totalBeersNeeded :: [Int]
totalBeersNeeded = (pure (*)) <*>  beersPerGuest <*> totalPeople

beersToPurchase :: [Int]
beersToPurchase = (pure (-)) <*> totalBeersNeeded  <*> remainingBeer
