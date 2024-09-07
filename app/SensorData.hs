{-# LANGUAGE OverloadedStrings #-}

module SensorData (SensorData(..)) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

-- Data structure
data SensorData = SensorData
    { date        :: String
    , time        :: String
    , temperature :: Double
    , humidity    :: Double
    } deriving (Show)

-- FromJSON instance for decoding JSON
instance FromJSON SensorData where
    parseJSON = withObject "SensorData" $ \v ->
        SensorData <$> v .: "date"
                   <*> v .: "time"
                   <*> v .: "temperature"
                   <*> v .: "humidity"