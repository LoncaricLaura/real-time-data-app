{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client         (newManager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Data.Aeson                  (eitherDecode)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

-- Defining the data structure
data SensorData = SensorData
    { date        :: String
    , time        :: String
    , temperature :: Double
    , humidity    :: Double
    } deriving (Show)

-- FromJSON instance for automatic JSON decoding
instance FromJSON SensorData where
    parseJSON = withObject "SensorData" $ \v ->
        SensorData <$> v .: "date"
                   <*> v .: "time"
                   <*> v .: "temperature"
                   <*> v .: "humidity"

-- fetching data
fetchData :: String -> IO (Either String [SensorData])
fetchData url = do
    manager <- newManager tlsManagerSettings  -- Use TLS for HTTPS connections
    request <- parseRequest url               -- Parse the request from the URL
    response <- httpLbs request manager       -- Perform the request and get the response
    let body = responseBody response
    return $ eitherDecode body                -- Parse the JSON response

main :: IO ()
main = do
    let apiUrl = "http://192.168.1.25/getlogs"
    result <- fetchData apiUrl
    case result of
        Left err -> putStrLn $ "Error parsing JSON: " ++ err
        Right sensorDataList -> do
            putStrLn "Received sensor data:"
            mapM_ print sensorDataList
