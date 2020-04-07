module Main where

import Network.HTTP.Simple
import Network.HTTP.Types.Status

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC


myToken :: BC.ByteString
myToken = "gGmslPXgMbTGVXBJQaBlhGEZBHRXqzWj"

noaaHost :: BC.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: BC.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest token host method path =
  setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "Token" [token]
  $ setRequestPath path
  $ setRequestSecure True
  $ setRequestPort 443
  $ defaultRequest

buildRequestNoSSL :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequestNoSSL token host method path =
  setRequestMethod method
  $ setRequestHost host
  $ setRequestHeader "Token" [token]
  $ setRequestPath path
  $ setRequestPort 443
  $ defaultRequest

req :: Request
req = buildRequest myToken noaaHost "GET" apiPath

main' :: IO ()
main' = do
  res <- httpLBS req
  let status = getResponseStatusCode res
  if status == 200
    then do
      putStrLn "Saving request to file"
      let jsonBody = getResponseBody res
      LB.writeFile "data.json" jsonBody
    else putStrLn "Request failed with error"

main :: IO ()
main = do
  res <- httpLBS req
  let status = getResponseStatus res
  if statusCode status == 200
    then do
      putStrLn "Saving request to file"
      let jsonBody = getResponseBody res
      LB.writeFile "data.json" jsonBody
    else putStrLn $
      show (statusCode status) ++
      ": " ++
      BC.unpack (statusMessage status)
