{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Glitch

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
