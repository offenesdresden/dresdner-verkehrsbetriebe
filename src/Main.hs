-- |
-- Module: Main
-- Copyright: (C) 2015 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Main where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.Transportation.Germany.DVB
import Network.Transportation.Germany.DVB.Route
import Network.Transportation.Germany.DVB.Monitor
import Options.Applicative

-- |Data structure containing all program options.
data ProgramOptions = ProgramOptions
  { poOrigin :: Location
  , poDestination :: Maybe Location
  , poOriginCity :: City
  , poDestinationCity :: City
  , poTime :: Maybe LocalTime
  , poTimeType :: Maybe TimeType
  , poOffset :: Integer
  , poLimit :: Integer
  }

-- |Program options parser.
programOptionsParser :: LocalTime -> Parser ProgramOptions
programOptionsParser currentTime =
  ProgramOptions <$>
    (Location <$>
     strOption (long "origin" <>
                help "Location in the city from which to travel.")) <*>
    (strToMaybeLocation <$>
     strOption (long "destination" <>
                value "" <>
                help ("Location in the city to which to travel. Omit to " ++
                      "monitor the origin stop."))) <*>
    (City <$>
     strOption (long "origin-city" <>
                value (show defaultCity) <>
                help "City from which to travel.")) <*>
    (City <$>
     strOption (long "destination-city" <>
                value (show defaultCity) <>
                help "City to which to travel.")) <*>
    (strToTime currentTime <$>
     strOption (long "time" <>
                value "now" <>
                help "Time of departure of arrival.")) <*>
    (strToTimeType <$>
     strOption (long "time-type" <>
                value (show DepartureTime) <>
                help "Departure (dep) or arrival (arr) time.")) <*>
    (read <$>
     strOption (long "offset" <>
                value "0" <>
                help "Offset of entries when monitoring the origin stop.")) <*>
    (read <$>
     strOption (long "limit" <>
                value "10" <>
                help "Limit of entries when monitoring the origin stop."))
  where
    -- |Given a string that either contains text or is blank, return either a
    -- location or nothing respectively.
    strToMaybeLocation :: String -> Maybe Location
    strToMaybeLocation "" = Nothing
    strToMaybeLocation str = Just $ Location str
    -- |Given the current time and a time string, return a time if possible.
    strToTime :: LocalTime -> String -> Maybe LocalTime
    strToTime now "now" = Just now
    strToTime _ strTime = parseTime defaultTimeLocale "%d/%m/%Y %H:%M" strTime
    -- |Given a time type ("dep" or "arr"), return a TimeType if possible.
    strToTimeType :: String -> Maybe TimeType
    strToTimeType "dep" = Just DepartureTime
    strToTimeType "arr" = Just ArrivalTime
    strToTimeType _ = Nothing

-- |Information about the program options parser.
programOptionsInfo :: LocalTime -> ParserInfo ProgramOptions
programOptionsInfo currentTime =
  info (programOptionsParser currentTime) fullDesc

-- |Main program function.
main :: IO ()
main = do
  timezone <- getCurrentTimeZone
  currentTime <- utcToLocalTime timezone <$> getCurrentTime
  execParser (programOptionsInfo currentTime) >>= optMain

-- |Main program function after options parsing.
optMain :: ProgramOptions -> IO ()
optMain opts =
  case programOptionsToRequest opts of
    Just (Left request) -> do
      result <- route request
      case result of
        Left err -> error ("The result is invalid: " ++ show err)
        Right route' -> putStrLn $ prettyRoute route'
    Just (Right request) -> do
      result <- monitor request
      case result of
        Left err -> error ("The result is invalid: " ++ show err)
        Right connections -> putStrLn $ prettyMonitor connections
    Nothing -> error "There are problems with the request."

-- |Generate a route or monitor request from program options.
programOptionsToRequest :: ProgramOptions ->
                           Maybe (Either RouteRequest MonitorRequest)
programOptionsToRequest opts =
  case (poTime opts, poTimeType opts) of
    (Just time, Just timeType) ->
      case poDestination opts of
        Just destination ->
          return $ Left $ RouteRequest {
            routeReqOrigin = poOrigin opts,
            routeReqDestination = destination,
            routeReqCityOrigin = poOriginCity opts,
            routeReqCityDestination = poDestinationCity opts,
            routeReqTime = time,
            routeReqTimeType = timeType
          }
        Nothing ->
          return $ Right $ MonitorRequest {
            monitorReqCity = poOriginCity opts,
            monitorReqStop = poOrigin opts,
            monitorReqOffset = poOffset opts,
            monitorReqLimit = poLimit opts
          }
    (_, _) -> Nothing

-- |Print a route to be displayed to the user.
prettyRoute :: Route -> String
prettyRoute route' = concat $ map prettyTrip $ routeTrips route'
  where
    prettyTrip trip =
      let l1 = "===== Strecke: =============================================="
          l2 = " Dauer: " ++ tripDuration trip
          rest = concat $ map prettyLeg $ tripLegs trip
      in l1 ++ "\n" ++ l2 ++ "\n" ++ rest
    prettyLeg leg =
      let l1 = " * Teilstrecke (Linie: " ++ legNumber leg ++ "): " ++
               legDesc leg
          rest = concat $ map prettyStop $ legStops leg
      in l1 ++ "\n" ++ rest
    prettyStop stop =
      let platform = if null $ stopPlatformName stop
                     then "" else " (Gleis: " ++ stopPlatformName stop ++ ")"
          arrival = case stopArrival stop of
            Just arr ->
              " (Ankunft am " ++
              formatTime defaultTimeLocale "%d.%m.%Y" (stopArrivalTime arr) ++
              " um " ++
              formatTime defaultTimeLocale "%H:%M" (stopArrivalTime arr) ++
              " mit [" ++ show (stopArrivalDelayMins arr) ++
              " Minuten] Verspätung)"
            Nothing -> ""
          departure = case stopDeparture stop of
            Just dep ->
              " (Abfahrt am " ++
              formatTime defaultTimeLocale "%d.%m.%Y" (stopDepartureTime dep) ++
              " um " ++
              formatTime defaultTimeLocale "%H:%M" (stopDepartureTime dep) ++
              " mit [" ++ show (stopDepartureDelayMins dep) ++
              " Minuten] Verspätung)"
            Nothing -> ""
          l1 = "   ~ Haltestelle: " ++ stopName stop ++ platform ++ arrival ++
               departure ++ "\n"
      in l1

-- |Print transit connections to be displayed to the user.
prettyMonitor :: [TransitConnection] -> String
prettyMonitor connections = concat $ map prettyConnection connections
  where
    prettyConnection connection =
      let l1 = "===== Verbindung: ==========================================="
          l2 = " " ++ transConnDesc connection ++
               " (Linie: " ++ transConnNumber connection ++ ")"
          l3 = " Kommt in: " ++ show (transConnArrivalMinutes connection) ++
               " Minuten"
      in l1 ++ "\n" ++ l2 ++ "\n" ++ l3 ++ "\n"
