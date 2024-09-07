{-# LANGUAGE OverloadedStrings #-}

module Main where

import SensorAPI              (getDataForYesterday, getDataForLast7Days, getDataForPreviousMonth)
import Data.Time              (getCurrentTime, utctDay)

main :: IO ()
main = do
    let apiUrl = "http://192.168.1.25/getlogs"
    
    -- Fetch data for yesterday
    putStrLn "Fetching data for yesterday..."
    resultYesterday <- getDataForYesterday apiUrl
    case resultYesterday of
        Left err -> putStrLn $ "Error fetching data for yesterday: " ++ err
        Right sensorDataYesterday -> do
            putStrLn "Yesterday's Data:"
            mapM_ print sensorDataYesterday

    -- Fetch data for the last 7 days
    putStrLn "Fetching data for the last 7 days..."
    resultLast7Days <- getDataForLast7Days apiUrl
    case resultLast7Days of
        Left err -> putStrLn $ "Error fetching data for the last 7 days: " ++ err
        Right sensorDataLast7Days -> do
            putStrLn "Last 7 Days' Data:"
            mapM_ print sensorDataLast7Days

    -- Fetch data for the previous month
    putStrLn "Fetching data for the previous month..."
    resultPreviousMonth <- getDataForPreviousMonth apiUrl
    case resultPreviousMonth of
        Left err -> putStrLn $ "Error fetching data for the previous month: " ++ err
        Right sensorDataPreviousMonth -> do
            putStrLn "Previous Month's Data:"
            mapM_ print sensorDataPreviousMonth
