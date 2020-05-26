{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Semigroup

txt :: T.Text
txt = "toto\ntiti\ntata"

myLines :: T.Text -> [T.Text]
myLines = T.splitOn "\n"

myUnlines :: [T.Text] -> T.Text
myUnlines = T.intercalate "\n" 

aName :: T.Text
aName = "Ema"

helloPerson :: T.Text -> T.Text
helloPerson name = mconcat ["Hello ", name, "!"]

main :: IO ()
main = do
   TIO.putStrLn "Hello! What's your name?"
   name <- TIO.getLine
   let statement = helloPerson name
   TIO.putStrLn statement

someInts :: TL.Text
someInts = "1\n2\n3"

toInts :: TL.Text -> [Int]
toInts text = map (read . TL.unpack) (TL.lines text)

otherMain :: IO ()
otherMain = do
  userInput <- TLIO.getContents
  let numbers = toInts userInput
  TLIO.putStrLn (TL.pack (show (sum numbers)))
  