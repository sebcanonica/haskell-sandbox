import System.IO
import System.Environment
import qualified Data.Text.IO as TI

hStuff = openFile "stuff.txt" ReadMode

mainTmp :: IO ()
mainTmp = do
   helloFile <- openFile "hello.txt" ReadMode
   isEOF <- hIsEOF helloFile
   firstLine <- if not isEOF
                then hGetLine helloFile
                else return "empty"
   isEOF <- hIsEOF helloFile
   putStrLn "done!"


main :: IO ()
main = do
    args <- getArgs
    let (src:dest:_) = args
    content <- TI.readFile src
    writeFile dest content
