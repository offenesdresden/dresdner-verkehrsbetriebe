-- |
-- Module: Main
-- Copyright: (C) 2015 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Main where

import Control.Applicative
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.Transportation.Germany.DVB
import Network.Transportation.Germany.DVB.Route
import Options.Applicative
import System.Locale

-- |Data structure containing all program options.
data ProgramOptions = ProgramOptions
  { poOrigin :: Location
  , poDestination :: Location
  , poOriginCity :: City
  , poDestinationCity :: City
  , poTime :: Maybe LocalTime
  , poTimeType :: Maybe TimeType
  }

-- |Program options parser.
programOptionsParser :: LocalTime -> Parser ProgramOptions
programOptionsParser currentTime =
  ProgramOptions <$>
    (Location <$>
     strOption (long "origin" <>
                help "Location in the city from which to travel.")) <*>
    (Location <$>
     strOption (long "destination" <>
                help "Location in the city to which to travel.")) <*>
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
                help "Departure (dep) or arrival (arr) time."))
  where
    -- |Given the current time and a time string, return a time if possible.
    strToTime :: LocalTime -> String -> Maybe LocalTime
    strToTime now "now" = Just now
    strToTime _ strTime = parseTime defaultTimeLocale "%d/%m/%y" strTime
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
  currentTime <- utcToLocalTime utc <$> getCurrentTime
  execParser (programOptionsInfo currentTime) >>= optMain

-- |Main program function after options parsing.
optMain :: ProgramOptions -> IO ()
optMain opts =
  case programOptionsToRequest opts of
    Just request -> do
      result <- route request
      case result of
        Left err -> error ("The result is invalid: " ++ show err)
        Right route' -> putStrLn $ prettyRoute route'
    Nothing -> error "There are problems with the request."

-- |Generate a route request from program options.
programOptionsToRequest :: ProgramOptions -> Maybe RouteRequest
programOptionsToRequest opts =
  case (poTime opts, poTimeType opts) of
    (Just time, Just timeType) -> Just $ RouteRequest {
        routeReqOrigin = poOrigin opts,
        routeReqDestination = poDestination opts,
        routeReqCityOrigin = poOriginCity opts,
        routeReqCityDestination = poDestinationCity opts,
        routeReqTime = time,
        routeReqTimeType = timeType
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

          l1 = "   ~ Haltestelle: " ++ stopName stop ++ platform ++ " um " ++
               stopDepartureTime stop ++ " mit " ++ stopDelayMins stop ++
               " Verspätung\n"
      in l1
