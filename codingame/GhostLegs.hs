import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import Data.List.Split (wordsBy)

simpleSample = [
    "A  B  C",
    "|  |  |",
    "|--|  |",
    "|  |--|",
    "|  |--|",
    "|  |  |",
    "1  2  3"
    ]

superSimple = [
    "A  B",
    "|  |",
    "1  2"
    ]

answer :: [String] -> [String]
answer maze = map conduitFor headers
    where conduitFor s = s ++ exitFor s
          exitFor s = dropFrom (tail maze) (headerToCol headers s)
          headers = getHeaders maze

dropFrom :: [String] -> Int -> String
dropFrom [lastLine] entry = getExitName lastLine entry
dropFrom (line:rest) entry
    | canGoLeft = dropFrom rest (entry-1)
    | canGoRight = dropFrom rest (entry+1)
    | otherwise = dropFrom rest entry
    where canGoLeft = entry > 0 && '-' == line !! (3*entry-1)
          canGoRight = entry < width && '-' == line !! (3*entry+1)
          width = length line `div` 3

headerToCol :: [String] -> String -> Int
headerToCol headers header = fromJust (elemIndex header headers)

getHeaders :: [String] -> [String]
getHeaders = words . head

getExitName :: String -> Int -> String
getExitName footers n = words footers !! n

{--solve (h:t) = zipWith (++) entryNames $ foldr f exitNames rows
  where 
      entryNames = words h
      exitNames = words $ last t
      rows = init t
      f rows exitNames = foldr permute exitNames connectorPositions
        where interColumns = wordsBy (== '|') rows
              connectorPositions = elemIndices "--" interColumns

permute i exitNames = h ++ b:a:t 
    where 
        (h,a:b:t) = splitAt i exitNames--}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    input_line <- getLine
    let input = words input_line
    let w = read (head input) :: Int
    let h = read (input!!1) :: Int
    maze <- replicateM h getLine
    mapM_ (hPutStrLn stderr) maze
    putStrLn "answer"