import Control.Monad
import Data.Char

squares :: [(Int,Int)]
squares = do
    n <- [1..10]
    return (n, n^2)

guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter p xs = do
    x <- xs
    guard (p x)
    return x

testGuardFilter = guardFilter even [1..10]

powersOfTwo :: Int -> [Int]
powersOfTwo n = [value^2 | value <- [1..n]]

powersOfTwoAndThree :: Int -> [(Int,Int)]
powersOfTwoAndThree n = [(powersOfTwo,powersOfThree)
                        | value <- [1 .. n]
                        , let powersOfTwo = 2^value
                        , let powersOfThree = 3^value]

someWords = ["brown","blue","pink","orange"]

q323 = ["Mr. " ++ capitalized | lowerWord <- someWords
                              , let capitalized = toUpper (head lowerWord) : tail lowerWord ]

validDate (2,date) = date <= 28
validDate (month,date) | even month && month <= 5 = date < 30
                       | odd month && month >= 9 = date < 30
                       | month <= 12 = date <= 31
                       | otherwise = False


calendarDays :: [(Int, Int)]
calendarDays = [ date | month <- [1..12]
                      , day <- [1..31]
                      , let date = (month, day)
                      , validDate date ]

monthEnds :: [Int]
monthEnds = [31,28,31,30,31,30,31,31,30,31,30,31]

dates :: [Int] -> [Int]
dates ends = [date| end <- ends, date <- [1 ..end ] ]

datesDo :: [Int] -> [Int]
datesDo ends = do
    end <- ends
    [1..end]

datesM :: [Int] -> [Int]
datesM ends = ends >>= (\end ->  [1..end] )


