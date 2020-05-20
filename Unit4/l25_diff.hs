{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

sampleUnicode :: T.Text
sampleUnicode = "ვეპხის ტყაოსანი შოთა რუსთაველი"

sampleAscii :: T.Text
sampleAscii = "toto"

calcDiff :: BC.ByteString -> Int
calcDiff bytes = bytesCount - charsCount
    where bytesCount = BC.length bytes
          charsCount = (T.length . E.decodeUtf8) bytes

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    bytes <- BC.readFile filename
    print (calcDiff bytes)