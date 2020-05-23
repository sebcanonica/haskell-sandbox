module Main where

import Data.Char (isDigit)

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n-1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM _ [] = []
myTakePM n (x:xs) = x : myTakePM (n-1) xs

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x

maximumFailure :: Int
maximumFailure = maximum []

succFailure :: Int
succFailure = succ (maxBound :: Int)

sumFailure :: Integer
sumFailure = sum [1,100000000..] -- out of memory, not really partial application

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> maybeHead xs
                              <*> myTakeSafer (n-1) (Just (tail xs))

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:_) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: String
charExample = "cat"

charExampleEmpty :: String
charExampleEmpty = ""

add1stAnd2nd :: [Int] -> Either String Int
--add1stAnd2nd (x:xs) = (+x) <$> eitherHead xs
--add1stAnd2nd _ = Left "not enough element"
add1stAnd2nd xs = (+) <$> first <*> second
    where first = eitherHead xs
          second = eitherHead (tail xs) -- if xs is empty, the first eitherHead fail first
{- add1stAnd2nd xs = (+) <$> second <*> first
    where first = eitherHead xs
          second = eitherHead (tail xs) -- if xs is empty, runtime exception as tail is evaluated first-}

maxN :: Int
maxN = 10000

primes :: [Int]
primes = sieve [2 .. 10000]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime:rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
   show TooLarge = "Value exceed max bound"
   show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
   | n < 2 = Left InvalidValue
   | n > maxN = Left TooLarge
   | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- read <$> getLine
  let result = isPrime n
  print (displayResult result)


addStrInts :: String -> String -> Either String Int
addStrInts s1 s2
    | notParsable s1 && notParsable s2 = Left "Neither value can't be parsed"
    | notParsable s1 = Left "First value can't be parsed"
    | notParsable s2 = Left "Second value can't be parsed"
    | otherwise = Right (read s1 + read s2)
    where notParsable = not . all isDigit

succMaybe :: (Enum a, Bounded a, Eq a) => a -> Maybe a
succMaybe x | x == maxBound = Nothing
            | otherwise = Just (succ x)

tailButSafe :: [a] -> [a]
tailButSafe (_:xs) = xs
tailButSafe _ = []

longerThan :: Int -> [a] -> Bool
longerThan n xs = (not.null) (drop n xs)

lastEither :: [a] -> Either String a
lastEither xs | null xs = Left "Empty list"
              | longerThan 1000000 xs = Left "Huge list"
              | otherwise = Right (last xs)

