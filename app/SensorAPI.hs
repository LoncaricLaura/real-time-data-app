{-# LANGUAGE OverloadedStrings #-}

module SensorAPI (fetchDataWithParams, getDataForYesterday, getDataForLast7Days, getDataForPreviousMonth) where

import Network.HTTP.Client         (newManager, httpLbs, parseRequest, responseBody)
import Network.HTTP.Client.TLS     (tlsManagerSettings)
import Data.Aeson                  (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.URI      (renderQuery)
import Data.Time (getCurrentTime, utctDay, addDays, fromGregorian, toGregorian)

import SensorData                  (SensorData(..))

-- Helper function to create query parameters in the correct format
buildQueryParams :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> String
buildQueryParams fromDate toDate fromTime toTime =
    let queryParams :: [(BS.ByteString, Maybe BS.ByteString)]
        queryParams = [ ("from_date", fmap BS.pack fromDate)
                      , ("to_date", fmap BS.pack toDate)
                      , ("from_time", fmap BS.pack fromTime)
                      , ("to_time", fmap BS.pack toTime)
                      ]
        queryString = renderQuery True (filter (\(_, v) -> v /= Nothing) queryParams)
    in BS.unpack queryString

-- Function to fetch data from API with dynamic parameters
fetchDataWithParams :: String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Either String [SensorData])
fetchDataWithParams baseUrl fromDate toDate fromTime toTime = do
    manager <- newManager tlsManagerSettings

    let queryString = buildQueryParams fromDate toDate fromTime toTime
    let fullUrl = baseUrl ++ queryString

    request <- parseRequest fullUrl
    response <- httpLbs request manager
    let body = responseBody response
    return $ eitherDecode body

-- Function to get data for yesterday
getDataForYesterday :: String -> IO (Either String [SensorData])
getDataForYesterday apiUrl = do
    today <- utctDay <$> getCurrentTime
    let yesterday = addDays (-1) today
    fetchDataWithParams apiUrl (Just $ show yesterday) (Just $ show yesterday) Nothing Nothing

-- Function to get data for the last 7 days
getDataForLast7Days :: String -> IO (Either String [SensorData])
getDataForLast7Days apiUrl = do
    today <- utctDay <$> getCurrentTime
    let endOfLast7Days = addDays (-1) today
        startOfLast7Days = addDays (-6) endOfLast7Days
    fetchDataWithParams apiUrl (Just $ show startOfLast7Days) (Just $ show endOfLast7Days) Nothing Nothing

-- Function to get data for the previous month
getDataForPreviousMonth :: String -> IO (Either String [SensorData])
getDataForPreviousMonth apiUrl = do
    today <- utctDay <$> getCurrentTime
    let (currentYear, currentMonth, _) = toGregorian today
        previousMonth = if currentMonth == 1 then 12 else currentMonth - 1
        previousYear = if currentMonth == 1 then currentYear - 1 else currentYear
        startOfPreviousMonth = fromGregorian previousYear previousMonth 1
        endOfPreviousMonth = fromGregorian previousYear previousMonth (daysInMonth previousYear previousMonth)
    fetchDataWithParams apiUrl (Just $ show startOfPreviousMonth) (Just $ show endOfPreviousMonth) Nothing Nothing

-- get the number of days in a month
daysInMonth :: Integer -> Int -> Int
daysInMonth year month = case month of
    2  -> if isLeapYear year then 29 else 28
    4 -> 30
    6 -> 30
    9 -> 30
    11 -> 30
    _ -> 31

-- check if a year is a leap year
isLeapYear :: Integer -> Bool
isLeapYear year = (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0)
