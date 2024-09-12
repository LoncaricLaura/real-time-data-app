{-# LANGUAGE OverloadedStrings #-}

module Main where

import SensorAPI (getDataForYesterday, getDataForLast7Days, getDataForPreviousMonth, fetchDataWithParams, fetchCurrentTemperature, saveSensorDataToFile)
import Data.Time (getCurrentTime, utctDay)
import Statistics (calculateStatistics, heatIndex)
import Chart (chartStatistics, chartHourlyStatistics)
import Histogram (plotTemperatureHistogram, plotHumidityHistogram)
import CurrentTempData
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    loadFile defaultConfig
    
    apiUrl <- lookupEnv "API_URL"
    currentTempUrl <- lookupEnv "CURRENT_TEMPERATURE_URL"

    let apiUrlVal = fromMaybe "API_URL not set" apiUrl
    let currentTempUrlVal = fromMaybe "CURRENT_TEMPERATURE_URL not set" currentTempUrl

    putStrLn $ "API URL: " ++ apiUrlVal
    putStrLn $ "Current Temperature URL: " ++ currentTempUrlVal

    -- Fetch all_time data
    putStrLn "Fetching all data..."
    resultAll <- fetchDataWithParams apiUrlVal Nothing Nothing Nothing Nothing
    case resultAll of
        Left err -> putStrLn $ "Error fetching all data: " ++ err
        Right sensorDataAll -> do
            putStrLn "All data:"
            -- mapM_ print sensorDataAll
            calculateStatistics sensorDataAll

    -- Fetch data for yesterday
    putStrLn "\nFetching data for yesterday..."
    resultYesterday <- getDataForYesterday apiUrlVal
    case resultYesterday of
        Left err -> putStrLn $ "Error fetching data for yesterday: " ++ err
        Right sensorDataYesterday -> do
            putStrLn "Yesterday's Data:"
            -- mapM_ print sensorDataYesterday
            plotTemperatureHistogram sensorDataYesterday "web/histograms/temperature_histogram_yesterday.png"
            plotHumidityHistogram sensorDataYesterday "web/histograms/humidity_histogram_yesterday.png" 
            calculateStatistics sensorDataYesterday
            chartHourlyStatistics sensorDataYesterday "Yesterday Temperature and Humidity" "web/charts/yesterday_statistics.svg"
    
    -- Fetch data for the last 7 days
    putStrLn "\nFetching data for the last 7 days..."
    resultLast7Days <- getDataForLast7Days apiUrlVal
    case resultLast7Days of
        Left err -> putStrLn $ "Error fetching data for the last 7 days: " ++ err
        Right sensorDataLast7Days -> do
            putStrLn "Last 7 Days' Data:"
            -- mapM_ print sensorDataLast7Days
            plotTemperatureHistogram sensorDataLast7Days "web/histograms/temperature_histogram_7Days.png"
            plotHumidityHistogram sensorDataLast7Days "web/histograms/humidity_histogram_7Days.png"    
            calculateStatistics sensorDataLast7Days
            chartStatistics sensorDataLast7Days "Weekly Temperature and Humidity" "web/charts/weekly_statistics.svg"

    -- Fetch data for the previous month
    putStrLn "\nFetching data for the previous month..."
    resultPreviousMonth <- getDataForPreviousMonth apiUrlVal
    case resultPreviousMonth of
        Left err -> putStrLn $ "Error fetching data for the previous month: " ++ err
        Right sensorDataPreviousMonth -> do
            putStrLn "Previous Month's Data:"
            -- mapM_ print sensorDataPreviousMonth
            plotTemperatureHistogram sensorDataPreviousMonth "web/histograms/temperature_histogram_PrevMonth.png"
            plotHumidityHistogram sensorDataPreviousMonth "web/histograms/humidity_histogram_PrevMonth.png"
            calculateStatistics sensorDataPreviousMonth
            chartStatistics sensorDataPreviousMonth "Monthly Temperature and Humidity" "web/charts/monthly_statistics.svg"

    putStrLn "\nFetching current temperature data..."
    result <- fetchCurrentTemperature currentTempUrlVal
    case result of
        Left err       -> putStrLn $ "Error: " ++ err
        Right currentTempResp -> do 
            let temp = temperature currentTempResp
                hums  = humidity currentTempResp
                _datetime = datetime currentTempResp
            putStrLn $ "Current temperature: " ++ show temp
            putStrLn $ "Current humidity: " ++ show hums
            putStrLn $ "Current datetime: " ++ show _datetime
            let heatIndexes = heatIndex temp hums
            putStrLn $ "Heat index - feels like: " ++ show heatIndexes ++ "°C"
            saveSensorDataToFile temp hums heatIndexes _datetime
