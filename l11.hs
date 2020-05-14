half :: Integer -> Double
half n = fromIntegral n / 2

halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (2*n)

-- makeAddress :: Int -> String -> String -> (Int, String, String)
-- makeAddressP1 :: String -> String -> (Int, String, String)
-- makeAddressP2 :: String -> (Int, String, String)

-- filter :: (a -> Bool) -> [a] -> [a]

-- myHead :: [a] -> a
-- myHead [] = [] -- can't be
-- myTail :: [a] -> [a]
-- myTail [] = [] -- Ok

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- real foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- myFoldl :: (t -> a -> t) -> t -> [a] -> t
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x