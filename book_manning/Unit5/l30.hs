import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          ,(2,"KINGinYELLOW")
                          ,(3,"dagon1997")
                          ,(4,"rcarter1919")
                          ,(5,"xCTHULHUx")
                          ,(6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         ,("KINGinYELLOW",15000)
                         ,("dagon1997",300)
                         ,("rcarter1919",12)
                         ,("xCTHULHUx",50000)
                         ,("yogSOThoth",150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits username = Map.lookup username creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just username) = lookupCredits username

creditsFromIdWrapped :: GamerId -> Maybe PlayerCredits
creditsFromIdWrapped id = altLookupCredits (lookupUserName id)

creditsFromIdStrange :: GamerId -> Maybe (Maybe PlayerCredits)
creditsFromIdStrange id = pure lookupCredits <*> lookupUserName id
--                                 (f func)  <*>  (f v)
--                        lookupCredits <$> lookupUserName id
--                             func     <$> (f v) -> f (res) and res = f resv so (f (f resv))

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n*2)

echoDouble :: IO ()
echoDouble = readInt >>= printDouble

ioPlus2 :: Num a => a -> IO a
ioPlus2 = (\x -> return ((+2) x))

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

helloName :: IO ()
helloName = askForName >>
            getLine >>=
            (\name -> return (nameStatement name)) >>=
            putStrLn

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM func mv = mv >>= return . func

testAllFmapM = allFmapM (+2) (Just 3)

-- fmap :: (a -> b) -> f a -> f b

-- (>>=) :: m a -> (a -> m b) -> m b
-- (>>) :: m a -> m b -> m b
-- return :: a -> m a
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf mv = mf >>= (\f -> mv >>= return . f)
--             mf >>= (\f -> f <$> mv)
--       m (t->u) ->  ( (t->u) -> m v )          -> m v
-- a = t->u ; b = v

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just v) f = f v