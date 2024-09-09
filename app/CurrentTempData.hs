{-# LANGUAGE OverloadedStrings #-}

module CurrentTempData where
-- module SensorData (SensorData(..)) where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))

data CurrentTempData = CurrentTempData
    { datetime    :: String
    , humidity    :: Double
    , temperature :: Double
    } deriving (Show)

instance FromJSON CurrentTempData where
    parseJSON = withObject "CurrentTempData" $ \v ->
        CurrentTempData <$> v .: "_datetime"
                            <*> v .: "humidity"
                            <*> v .: "temperature"