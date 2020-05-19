import qualified Data.Map as Map

helloPerson :: String -> String  -- pure world
helloPerson name = "Hello" ++ " " ++ name ++ "!"

main :: IO () -- IO impure world / IO () = void
main = do
   putStrLn "Hello! What's your name?"
   name <- getLine -- :: IO String assigned to a normal string
   let statement = helloPerson name -- create a non IO variable
   putStrLn statement -- :: String -> IO ()

argInput :: Map.Map Int String
argInput = Map.fromList [(1,"World")]

mainMaybe :: Maybe String
mainMaybe = do
    name <- Map.lookup 1 argInput
    let statement = helloPerson name
    return statement

mainFib :: IO Int
mainFib = do
    putStrLn "How many Fib?"
    n <- getLine
    let number = fib (read n)
    return number

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

    