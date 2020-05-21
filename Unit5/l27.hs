import qualified Data.Map as Map

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just s) = Just (reverse s)

aMaybeString :: Maybe String
aMaybeString = Just "toto"

aReverseMaybeString = reverse <$> Just "toto"


data RobotPart = RobotPart
   { name :: String
   , description :: String
   , cost :: Double
   , count :: Int
   } deriving Show

leftArm :: RobotPart
leftArm  = RobotPart
   { name = "left arm"
   , description = "left arm for face punching!"
   , cost = 1000.00
   , count = 3
   }

rightArm :: RobotPart
rightArm  = RobotPart
   { name = "right arm"
   , description = "right arm for kind hand gestures"
   , cost = 1025.00
   , count = 5
   }

robotHead :: RobotPart
robotHead  = RobotPart
   { name = "robot head"
   , description = "this head looks mad"
   , cost = 5092.25
   , count = 2
   }


partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
 where keys = [1,2,3]
       vals = [leftArm,rightArm,robotHead]
       keyVals = zip keys vals


allParts :: [RobotPart]
--allParts = map snd (Map.toList partsDB)
allParts = snd <$> Map.toList partsDB

newtype Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box v) = Box (f v)

morePresents :: Int -> Box a -> Box [a]
morePresents n b = replicate n <$> b

myBox :: Box Int
myBox = Box 1

wrapped = fmap Box myBox

unwrap (Box v) = v

unwrapped = fmap unwrap wrapped

costFor :: String -> Maybe Double
costFor id = cost <$> part
    where
        numId = read id :: Int
        part = Map.lookup numId partsDB

printCost :: Maybe Double -> IO()
printCost Nothing = putStrLn "item not found"
printCost (Just cost)= print cost

main :: IO()
main = do
    id <- getLine
    let cost = costFor id
    printCost cost
