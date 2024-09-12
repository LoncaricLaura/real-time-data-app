{-# LANGUAGE OverloadedStrings #-}

module Chart (chartStatistics, chartHourlyStatistics) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Data.Time (Day, TimeOfDay(..), parseTimeM, defaultTimeLocale, utctDay, addDays, getCurrentTime)
import Data.List (groupBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import SensorData (SensorData(..))
import Statistics (average)

parseDate :: String -> Maybe Day
parseDate dateStr = parseTimeM True defaultTimeLocale "%Y-%m-%d" dateStr

aggregateByDay :: [SensorData] -> [(Day, Double, Double)]
aggregateByDay sensorData = 
    let
        dayTempHumidity = [(fromMaybe (error "Invalid date") (parseDate (date sd)), temperature sd, humidity sd) | sd <- sensorData]
        
        groupedByDay = groupBy ((==) `on` fst3) dayTempHumidity
        
        aggregateDayStats dayData = 
            let
                temperatures = map (\(_, t, _) -> t) dayData
                humidities = map (\(_, _, h) -> h) dayData
                avgTemp = average temperatures
                avgHum = average humidities
            in (fst3 (head dayData), avgTemp, avgHum)

        fst3 (x, _, _) = x
    in map aggregateDayStats groupedByDay

parseTime :: String -> Maybe TimeOfDay
parseTime = parseTimeM True defaultTimeLocale "%H:%M"

aggregateByHour :: [SensorData] -> [(Int, Double, Double)]
aggregateByHour sensorData = 
    let
        hourTempHumidity = [(fromMaybe (error "Invalid time") (parseTime (time sd)), temperature sd, humidity sd) | sd <- sensorData]
        
        groupedByHour = groupBy ((==) `on` (todHour . fst3)) hourTempHumidity
        
        aggregateHourStats hourData =
            let
                temperatures = map (\(_, t, _) -> t) hourData
                humidities = map (\(_, _, h) -> h) hourData
                avgTemp = average temperatures
                avgHum = average humidities
            in (todHour (fst3 (head hourData)), avgTemp, avgHum)
        
        fst3 (x, _, _) = x
        todHour (TimeOfDay h _ _) = h

    in map aggregateHourStats groupedByHour

hourLabels :: [String]
hourLabels = map show [0..23]

createHourlyLayout :: [SensorData] -> String -> FilePath -> IO ()
createHourlyLayout sensorData title filePath = do
    let stats = aggregateByHour sensorData
    toFile def filePath $ do
        layoutlr_title .= title
        layoutlr_x_axis . laxis_title .= "Hour"
        layoutlr_x_axis . laxis_generate .= autoIndexAxis hourLabels

        layoutlr_left_axis . laxis_title .= "Temperature (°C)"
        layoutlr_right_axis . laxis_title .= "Humidity (%)"
        plotLeft $ line "Average Temperature" [map (\(t, avgT, _) -> (t, avgT)) stats]
        
        plotRight $ line "Average Humidity" [map (\(t, _, avgH) -> (t, avgH)) stats]

createChartLayout :: [SensorData] -> String -> FilePath -> IO ()
createChartLayout sensorData title filePath = do
    let stats = aggregateByDay sensorData
    toFile def filePath $ do
        layoutlr_title .= title
        layoutlr_x_axis . laxis_title .= "Date"
        layoutlr_left_axis . laxis_title .= "Temperature (°C)"
        layoutlr_right_axis . laxis_title .= "Humidity (%)"
        
        plotLeft $ line "Average Temperature" [map (\(d, avgT, _) -> (d, avgT)) stats]
        
        plotRight $ line "Average Humidity" [map (\(d, _, avgH) -> (d, avgH)) stats]

chartStatistics :: [SensorData] -> String -> String -> IO ()
chartStatistics sensorData title path = do 
    putStrLn ("Chart - '" ++ title ++ "' saving into '" ++ path ++ "'")
    createChartLayout sensorData title path

chartHourlyStatistics :: [SensorData] -> String -> String -> IO ()
chartHourlyStatistics sensorData title path = do
    putStrLn ("Chart - '" ++ title ++ "' saving into '" ++ path ++ "'")
    createHourlyLayout sensorData title path