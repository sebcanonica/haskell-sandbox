module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

data Name = Name
    { firstName :: T.Text
    , lastName :: T.Text
    } deriving (Show{- , Generic -})

{- instance FromJSON Name
instance ToJSON Name -}

instance FromJSON Name where
    parseJSON (Object v) = Name
        <$> v .: "firstName"
        <*> v .: "lastName"

instance ToJSON Name where
    toJSON (Name firstName lastName) =
        object [ "firstName" .= firstName
               , "lastName" .= lastName
        ]

sampleName = Name "Ema" "Nymton"
encodedName = encode sampleName
sampleJSON = "{\"lastName\":\"Nymton\",\"firstName\":\"Ema\"}"
decodedName = decode sampleJSON :: Maybe Name

data NOAAResult = NOAAResult
                  { uid :: T.Text
                  , mindate :: T.Text
                  , maxdate :: T.Text
                  , name :: T.Text
                  , datacoverage :: Double
                  , resultId :: T.Text
                  } deriving Show

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult <$> v .: "uid"
               <*> v .: "mindate"
               <*> v .: "maxdate"
               <*> v .: "name"
               <*> v .: "datacoverage"
               <*> v .: "id"

instance ToJSON NOAAResult where
    toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
        object [ "uid" .= uid
               , "mindate" .= mindate
               , "maxdate" .= maxdate]

data Resultset = Resultset
                 { offset :: Int
                 , count :: Int
                 , limit :: Int
                 } deriving (Show,Generic)

instance FromJSON Resultset
instance ToJSON Resultset

newtype Metadata = Metadata
                {
                  resultset :: Resultset
                } deriving (Show,Generic)

instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResponse = NOAAResponse
                    { metadata :: Metadata
                    , results :: [NOAAResult]
                    } deriving (Show,Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = forM_ results (print . name)

printResultsEither :: Either String [NOAAResult] -> IO ()
printResultsEither (Left msg) = print ("error " ++ msg)
printResultsEither (Right results) = forM_ results (print . name)

main :: IO ()
main = do
       jsonData <- B.readFile "data.json"
       let noaaResponse = eitherDecode jsonData :: Either String NOAAResponse
       let noaaResults = results <$> noaaResponse
       printResultsEither noaaResults
       print (encode noaaResults)



intListExample :: IntList
intListExample = Cons 1 $
                 Cons 2 EmptyList

data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)

instance FromJSON IntList
instance ToJSON IntList

encodedIntList = encode intListExample
decodedIntList :: Maybe IntList
decodedIntList = decode "{\"tag\":\"Cons\",\"contents\":[1,{\"tag\":\"Cons\",\"contents\":[2,{\"tag\":\"EmptyList\"}]}]}"