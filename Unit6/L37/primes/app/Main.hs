module Main where

import Primes

outputPrime :: Maybe Bool -> String
outputPrime Nothing = "Sorry, this number is not a valid candidate for primality testing"
outputPrime (Just True) = "It is prime!"
outputPrime (Just False) = "It is not prime..."

main :: IO ()
main = do
    putStrLn "Enter a number to check if it's prime:"
    n <- read <$> getLine :: IO Int
    putStrLn ((outputPrime . isPrime) n)
