module Statistics (calculateStatistics) where

import SensorData (SensorData(..))

-- Function to calculate and print basic statistics (Average, Min, Max)
calculateStatistics :: [SensorData] -> IO ()
calculateStatistics sensorDataList = do
    let temps = map temperature sensorDataList
        hums  = map humidity sensorDataList

    putStrLn $ "Average Temperature: " ++ show (average temps) ++ "°C"
    putStrLn $ "Max Temperature: " ++ show (maximum temps) ++ "°C"
    putStrLn $ "Min Temperature: " ++ show (minimum temps) ++ "°C"

    putStrLn $ "Average Humidity: " ++ show (average hums) ++ "%"
    putStrLn $ "Max Humidity: " ++ show (maximum hums) ++ "%"
    putStrLn $ "Min Humidity: " ++ show (minimum hums) ++ "%"

-- Helper function to calculate average
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)