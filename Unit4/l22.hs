import Control.Monad
import Data.List.Split
import qualified Data.Map as Map

main :: IO ()
main = do
    putStrLn "Enter 3 things"
    lines <- mapM (const getLine) [1..3]
    mapM_ putStrLn lines
    putStrLn "done"

myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n f = mapM (const f) [1..n]

main4 :: IO ()
main4 = do
    putStrLn "Enter 4 things"
    lines <- myReplicateM 4 getLine
    mapM_ putStrLn lines
    putStrLn "done"

mainReverse :: IO ()
mainReverse = do
  userInput <- getContents
  print (reverse userInput)

toInts :: String -> [Int]
toInts = map read . lines

mainSum :: IO ()
mainSum = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum numbers)

mainSquare :: IO ()
mainSquare = do
  userInput <- getContents
  let numbers = toInts userInput
  print (sum (map (^2) numbers))


evaluate :: String -> Int
evaluate s 
        | '+' `elem` s = evaluateInternals "+" (+) s
        | '*' `elem` s = evaluateInternals "*" (*) s
        | otherwise = 0
    where evaluateInternals operatorChar operator s = operator a b
            where operands = splitOn operatorChar s
                  a = (read . head) operands
                  b = (read . head) (tail operands)

addResult = evaluate "1+2"
mulResult = evaluate "3*4"

simpleCalc :: IO ()
simpleCalc = do
    input <- getContents
    let list = lines input
    let results = map evaluate list
    mapM_ print results
    
calc :: [String] -> Int
calc (val1:"+":val2:rest) = read val1 + read val2
calc (val1:"*":val2:rest) = read val1 * read val2

mainCalc :: IO ()
mainCalc = do
  userInput <- getContents
  let values = lines userInput
  print (calc values)

--quotes = Map.fromList [(1, "quote1"), (2, "quote2"), (3, "quote3"), (4, "quote4"), (5, "quote5")]
quotes = ["quote 1","quote 2","quote 3","quote 4","quote 5"]

getQuote :: [String] -> [String]
getQuote [] = []
getQuote ("n":xs) = []
getQuote (x:xs) = quote : getQuote xs
    where quote = quotes !! (read x -1)

mainQuote :: IO()
mainQuote = do
    input <- getContents
    let values = lines input
    mapM_ putStrLn (getQuote values)
