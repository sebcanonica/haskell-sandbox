import qualified Data.Map as Map

type LatLong = (Double,Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham",(42.6054,-70.7829))
                          ,("Innsmouth",(42.8250,-70.8150))
                          ,("Carcosa",(29.9714,-90.7694))
                          ,("New York",(40.7776,-73.9691))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double,Double)
latLongToRads (lat,long) = (rlat,rlong)
 where rlat = toRadians lat
       rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius * c
 where (rlat1,rlong1) = latLongToRads coords1
       (rlat2,rlong2) = latLongToRads coords2
       dlat = rlat2 - rlat1
       dlong = rlong2 - rlong1
       a = sin (dlat/2)^2 + cos rlat1 * cos rlat2 * sin (dlong/2)^2
       c = 2 * atan2 (sqrt a) (sqrt (1-a))
       earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just distance) = putStrLn (show distance ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe (Just a) (Just b) = Just (a+b)
addMaybe _ _ = Nothing

-- distanceFromNY = haversine newYork

val1 = Just 10
val2 = Just 5
val3 = Just 2

mulVal = (*) <$> val1 <*> val2
divVal = div <$> val1 <*> val2
modVal = mod <$> val1 <*> val2

mulAll a b c = a*b*c
mulAllVal = mulAll <$> val1 <*> val2 <*> val3

minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree val1 val2 val3 = min val1 (min val2 val3)

minMaybe = minOfThree <$> Just 10 <*> Just 3 <*> Just 6

data User = User
   { uname :: String
   , gamerId :: Int
   , score :: Int
   } deriving Show


readInt :: IO Int
readInt = read <$> getLine

main :: IO ()
main = do
   putStrLn "Enter a username, gamerId and score"
   user <- User <$> getLine <*> readInt <*> readInt
   print user

missingNameUser = User <$> Nothing <*> Just 1 <*> Just 234

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO a b = do
    aRaw <- a
    bRaw <- b
    return (haversine aRaw bRaw)

haversineIO2 :: IO LatLong -> IO LatLong -> IO Double
haversineIO2 a b = do
    aRaw <- a
    haversine aRaw <$> b

haversineIO3 :: IO LatLong -> IO LatLong -> IO Double
haversineIO3 a b = haversine <$> a <*> b

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

robotTorso :: RobotPart
robotTorso  = RobotPart
   { name = "robot torso"
   , description = "muscular torso"
   , cost = 6000.25
   , count = 3
   }

robotLeg :: RobotPart
robotLeg  = RobotPart
   { name = "robot leg"
   , description = "work on both sides"
   , cost = 16000.25
   , count = 32
   }

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
 where keys = [1..5]
       vals = [leftArm,rightArm,robotHead,robotTorso,robotLeg]
       keyVals = zip keys vals

cheapest :: RobotPart -> RobotPart -> RobotPart
cheapest p1 p2 = if cost p1 < cost p2
                 then p1
                 else p2

minCostPart :: Int -> Int -> Maybe RobotPart
minCostPart id1 id2 = cheapest <$> p1 <*> p2
    where p1 = Map.lookup id1 partsDB
          p2 = Map.lookup id2 partsDB

mainRobot :: IO()
mainRobot = do
    id1 <- readInt
    id2 <- readInt
    print (minCostPart id1 id2)