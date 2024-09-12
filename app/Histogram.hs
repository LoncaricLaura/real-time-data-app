{-# LANGUAGE OverloadedStrings #-}

module Histogram (plotTemperatureHistogram, plotHumidityHistogram) where

import Plots
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (renderRasterific)
import SensorData (SensorData(..))
import Data.List (sort)

extractData :: (SensorData -> Double) -> [SensorData] -> [Double]
extractData accessor = map accessor

plotHistogram :: String -> [Double] -> FilePath -> IO ()
plotHistogram title dataValues filePath = do
    let sortedData = sort dataValues

    let histogramAxis = r2Axis &~ do
            histogramPlot sortedData $ do
                key title
                plotColor .= blue
                areaStyle . _opacity .= 0.5

    renderRasterific filePath (dims2D 600 400) $ renderAxis histogramAxis

plotTemperatureHistogram :: [SensorData] -> FilePath -> IO ()
plotTemperatureHistogram sensorData filePath = do
    let temperatures = extractData temperature sensorData
    plotHistogram "Temperature Distribution" temperatures filePath

plotHumidityHistogram :: [SensorData] -> FilePath -> IO ()
plotHumidityHistogram sensorData filePath = do
    let humidities = extractData humidity sensorData
    plotHistogram "Humidity Distribution" humidities filePath
