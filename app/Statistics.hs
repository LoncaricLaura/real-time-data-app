module Statistics (calculateStatistics) where

import SensorData (SensorData(..))
import Data.List (genericLength)

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

    putStrLn $ "Mean Humidity: " ++ show (mean hums) ++ "°C"
    putStrLn $ "Standard deviation Humidity: " ++ show (standardDeviation hums)
    putStrLn $ "Coeffiecient of variation Humidity: " ++ show (coefficientOfVariation hums) ++ "%"
    
    putStrLn $ "Mean of Temperature: " ++ show (mean temps) ++ "°C"
    putStrLn $ "Standard deviation Temperature: " ++ show (standardDeviation temps)
    putStrLn $ "Coeffiecient of variation Temperature: " ++ show (coefficientOfVariation temps) ++ "%"
    
-- Helper function to calculate average
average :: [Double] -> Double
average xs = sum xs / fromIntegral (length xs)

mean :: [Double] -> Double
mean xs = sum xs / genericLength xs

variance :: [Double] -> Double
variance xs = let m = mean xs
              in sum (map (\x -> (x - m) ^ 2) xs) / genericLength xs

standardDeviation :: [Double] -> Double
standardDeviation xs = sqrt (variance xs)

coefficientOfVariation :: [Double] -> Double
coefficientOfVariation xs = let sd = standardDeviation xs
                                m  = mean xs
                            in sd / m * 100