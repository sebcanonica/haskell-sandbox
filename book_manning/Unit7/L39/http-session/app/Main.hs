module Main where

import ApiToken
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types.Status

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
         print "saving request to file"
         let jsonBody = getResponseBody response
         L.writeFile "data.json" jsonBody
    else print $ statusMessage $ getResponseStatus response -- not reached all the time as httpLBS throws

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
             -> BC.ByteString -> Request
buildRequest token host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestHeader "token" [token]
                                  $ setRequestPath path
                                  $ setRequestSecure True
                                  $ setRequestPort 443 defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

buildRequestNOSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString
                  -> BC.ByteString -> Request
buildRequestNOSSL token host method path  = setRequestMethod method
                                  $ setRequestHost host
                                  $ setRequestHeader "token" [token]
                                  $ setRequestPath path
                                  $ setRequestPort 443 defaultRequest

requestNOSSL :: Request
requestNOSSL = buildRequestNOSSL myToken noaaHost "GET" apiPath

--getStatusMessage (Status code message) = message