import qualified Data.Map as Map

newtype Box t = Box t deriving Show

boxMap :: (a -> b) -> Box a -> Box b
boxMap f (Box v) = Box (f v)

startBox = Box 12
endBox = boxMap show startBox

data Triple a = Triple a a a deriving Show

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap f (Triple v t u) = Triple (f v) (f t) (f u)

startTriple = Triple 1 2 3
endTriple = tripleMap show startTriple

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart,Heart,Brain,Spleen,Spleen,Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organPairs :: [(Int,Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map.Map Int Organ -> Map.Map Organ Int
organInventory oc = Map.fromList (zip allOrgans organsCount)
    where allOrgans = [Heart ..]
          organsFromCatalog = map snd (Map.toList oc)
          countOrgan = \o -> (length . filter (==o)) organsFromCatalog
          organsCount = map countOrgan allOrgans
