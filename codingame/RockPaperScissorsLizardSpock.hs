import System.IO
import Control.Monad
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    input_line <- getLine
    let n = read input_line :: Int
    players <- replicateM n getLine
    --mapM_ (hPutStrLn stderr) players
    let answers = solve players
    mapM_ putStrLn answers
    {-replicateM n $ do
        input_line <- getLine
        let input = words input_line
        let numplayer = read (input!!0) :: Int
        let signplayer = input!!1
        return ()-}
    
    -- hPutStrLn stderr "Debug messages..."
    return ()

data Player = Player {
    numPlayer :: Int,
    sign :: Sign,
    wins :: [Int]
} deriving Show

data Sign = R | P | C | L | S deriving (Show, Read, Eq)

solve :: [String] -> [String]
solve players = [ (show.numPlayer) winner, opponents]
    where 
        winner = playChampionship $ inputToPlayers players
        opponents = intercalate " " $ map show reverseWins
        reverseWins = reverse $ wins winner

inputToPlayers :: [String] -> [Player]
inputToPlayers = map lineToPlayer
    where 
        lineToPlayer line = Player numPlayer sign []
            where chars = words line
                  numPlayer = read $ head chars
                  sign = read $ last chars

playChampionship :: [Player] -> Player
playChampionship [winner] = winner
playChampionship players = playChampionship $ playRound players
    where playRound [p1,p2] = [matchWinner p1 p2]
          playRound (p1:p2:ps) = matchWinner p1 p2 : playRound ps

matchWinner :: Player -> Player -> Player
matchWinner (Player num1 sign1 wins1) (Player num2 sign2 wins2) 
    | p1Win = Player num1 sign1 (num2:wins1) 
    | otherwise = Player num2 sign2 (num1:wins2)
    where
        p1Win = (sign1 == sign2 && num1 < num2) || (sign1 /= sign2 && winningSign sign1 sign2 == sign1)

winningSign :: Sign -> Sign -> Sign
winningSign C P = C
winningSign P R = P
winningSign R L = R
winningSign L S = L
winningSign S C = S
winningSign C L = C
winningSign L P = L
winningSign P S = P
winningSign S R = S
winningSign R C = R
winningSign a b | a==b = a
                | otherwise = winningSign b a

sample = [
    "4 R",
    "1 P",
    "8 P",
    "3 R",
    "7 C",
    "5 S",
    "6 L",
    "2 L" ]

singlePlayer = head $ inputToPlayers sample
c1 = playChampionship [singlePlayer,singlePlayer]
c2 = playChampionship [singlePlayer,singlePlayer,singlePlayer,singlePlayer]
