{-# LANGUAGE OverloadedStrings #-}

module SensorAPI (fetchDataWithParams) where

import Network.HTTP.Client         (newManager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Data.Aeson                  (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.URI      (renderQuery)

import SensorData                  (SensorData(..))

-- Helper function to create query parameters in the correct format
buildQueryParams :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> String
buildQueryParams fromDate toDate fromTime toTime =
    let queryParams :: [(BS.ByteString, Maybe BS.ByteString)]
        queryParams = [ ("from_date", fmap BS.pack fromDate)
                      , ("to_date", fmap BS.pack toDate)
                      , ("from_time", fmap BS.pack fromTime)
                      , ("to_time", fmap BS.pack toTime)
                      ]
        queryString = renderQuery True (filter (\(_, v) -> v /= Nothing) queryParams)
    in BS.unpack queryString

-- Function to fetch data from API with dynamic parameters
fetchDataWithParams :: String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Either String [SensorData])
fetchDataWithParams baseUrl fromDate toDate fromTime toTime = do
    manager <- newManager tlsManagerSettings

    -- make query string and append it to the base URL
    let queryString = buildQueryParams fromDate toDate fromTime toTime
    let fullUrl = baseUrl ++ queryString

    -- Make request
    request <- parseRequest fullUrl
    response <- httpLbs request manager
    let body = responseBody response
    return $ eitherDecode body
