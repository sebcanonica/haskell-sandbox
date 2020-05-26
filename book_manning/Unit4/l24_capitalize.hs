import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TI

{-
Hello world!
Good bye world!
-}

main :: IO ()
main = do
    args <- getArgs
    let src = head args
    content <- TI.readFile src
    TI.writeFile src (T.toUpper content)
