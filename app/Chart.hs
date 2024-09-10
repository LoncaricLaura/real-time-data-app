{-# LANGUAGE OverloadedStrings #-}

module Chart (chartWeeklyStatistics, chartMonthlyStatistics) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay)
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


chartStatistics :: [SensorData] -> String -> FilePath -> IO ()
chartStatistics sensorData title filePath = do
    let stats = aggregateByDay sensorData
    toFile def filePath $ do
        layoutlr_title .= title
        layoutlr_x_axis . laxis_title .= "Date"
        layoutlr_left_axis . laxis_title .= "Temperature (°C)"
        layoutlr_right_axis . laxis_title .= "Humidity (%)"
        
        plotLeft $ line "Average Temperature" [map (\(d, avgT, _) -> (d, avgT)) stats]
        
        plotRight $ line "Average Humidity" [map (\(d, _, avgH) -> (d, avgH)) stats]

chartWeeklyStatistics :: [SensorData] -> IO ()
chartWeeklyStatistics sensorData = chartStatistics sensorData "Weekly Temperature and Humidity" "charts/weekly_statistics.svg"

chartMonthlyStatistics :: [SensorData] -> IO ()
chartMonthlyStatistics sensorData = chartStatistics sensorData "Monthly Temperature and Humidity" "charts/monthly_statistics.svg"
