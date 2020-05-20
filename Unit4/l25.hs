{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import System.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as E

bcInt :: BC.ByteString
bcInt = "6"

bcToto = "totototo" :: BC.ByteString

bcToInt :: BC.ByteString -> Int
bcToInt = read . BC.unpack

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReverseSection]

main :: IO ()
main = do
   args <- getArgs
   let fileName = head args
   imageFile <- BC.readFile fileName
   glitched <- foldM (\bytes func -> func bytes) imageFile glitchActions
   let glitchedFileName = mconcat ["glitched_",fileName]
   BC.writeFile glitchedFileName glitched
   print "all done"

intToChar :: Int -> Char
intToChar int =  toEnum safeInt
    where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before,newChar,after]
  where (before,rest) = BC.splitAt loc bytes
        after = BC.drop 1 rest
        newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1,bytesLength)
  charVal <- randomRIO (0,255)
  return (replaceByte location charVal bytes)

randomChar :: IO Char
randomChar = randomRIO ('a', 'z')

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before,changed,after]
  where (before,rest) = BC.splitAt start bytes
        (target,after) = BC.splitAt size rest
        changed =  BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0,bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before,reversed,after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          reversed = BC.reverse target

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection bytes = do
    let sectionSize = 30
    let bytesLength = BC.length bytes
    start <- randomRIO (0,bytesLength-sectionSize)
    return (reverseSection start sectionSize bytes)