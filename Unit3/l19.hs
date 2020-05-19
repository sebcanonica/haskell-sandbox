import qualified Data.Map as Map
import Data.Maybe

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

numOrZero :: Maybe Int -> Int
numOrZero (Just n) = n
numOrZero _ = 0

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
   show (Vat organ) = show organ ++ " in a vat"
   show (Cooler organ) = show organ ++ " in a cooler"
   show (Bag organ) = show organ ++ " in a bag"
data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = Vat Brain
organToContainer Heart = Cooler Heart
organToContainer organ = Bag organ

placeInLocation :: Container -> (Location,Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

process :: Organ -> (Location, Container)
process organ =  placeInLocation (organToContainer organ)

report :: (Location,Container) -> String
report (location,container) = show container ++
                              " in the " ++
                              show location

processAndReport :: Maybe Organ -> String
processAndReport (Just organ) = report (process organ)
processAndReport  Nothing = "error, id not found"

processRequest :: Int -> Map.Map Int Organ -> String
processRequest id catalog = processAndReport organ
 where organ = Map.lookup id catalog

reportMaybe :: Maybe (Location, Container) -> String
reportMaybe (Just lc) = report lc
reportMaybe _ = "container not found"

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
    where getContents = \id -> Map.lookup id catalog

{-emptyDrawers :: [Int] -> Map.Map Int Organ -> Int
emptyDrawers ids catalog = length emptyDrawer
    where emptyDrawer = filter isNothing allDrawersContent
          allDrawersContent = map getContent ids
          getContent = (`Map.lookup` catalog)-}

drawerContents = getDrawerContents possibleDrawers organCatalog

emptyDrawers :: [Maybe Organ] -> Int
emptyDrawers dc = length (filter isNothing dc)

maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap _ [] = []
maybeMap f (Nothing : xs) = Nothing : maybeMap f xs
maybeMap f (Just x : xs) = Just (f x) : maybeMap f xs