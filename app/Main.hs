{-# LANGUAGE OverloadedStrings #-}

module Main where

import SensorAPI              (fetchDataWithParams)
import Statistics             (calculateStatistics)
import SensorData             (SensorData(..))

main :: IO ()
main = do
    let apiUrl = "http://192.168.1.25/getlogs"
    
    -- Fetch data (for specific date, time)
    result <- fetchDataWithParams apiUrl (Just "2024-07-01") (Just "2024-07-31") Nothing Nothing

    case result of
        Left err -> putStrLn $ "Error parsing JSON: " ++ err
        Right sensorDataList -> do
            putStrLn "Received sensor data:"
            mapM_ print sensorDataList
            -- print statistics
            calculateStatistics sensorDataList
