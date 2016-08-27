-- |
-- Module: Network.Transportation.Germany.DVB.Route
-- Copyright: (C) 2015 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Network.Transportation.Germany.DVB.Route
( RouteRequest(..)
, RouteResult
, Route(..)
, Trip(..)
, Leg(..)
, Stop(..)
, StopArrival(..)
, StopDeparture(..)
, Error(..)
, route
) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Functor
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Network.HTTP
import Network.Stream
import Network.Transportation.Germany.DVB
import qualified Network.Transportation.Germany.DVB.Route.JSON as JSON
import System.Locale hiding (defaultTimeLocale)

-- |All data sent to DVB to query routes.
data RouteRequest = RouteRequest
  { routeReqOrigin :: Location
  , routeReqDestination :: Location
  , routeReqCityOrigin :: City
  , routeReqCityDestination :: City
  , routeReqTime :: LocalTime
  , routeReqTimeType :: TimeType
  } deriving (Show)

-- |Either route data or an error.
type RouteResult = Either Error Route

-- |All route data returned from a routing request.
data Route = Route { routeTrips :: [Trip] } deriving (Show)

-- |One of the possible trips to get from the origin to the destination.
data Trip = Trip
  { tripDuration :: String
  , tripLegs :: [Leg]
  } deriving (Show)

-- |One segment of a trip (for example: one vehicle).
data Leg = Leg
  { legNumber :: String
  , legDesc :: String
  , legStops :: [Stop]
  } deriving (Show)

-- |A stop that the vehicle makes in a leg.
data Stop = Stop
  { stopName :: String
  , stopPlatformName :: String
  , stopArrival :: Maybe StopArrival
  , stopDeparture :: Maybe StopDeparture
  } deriving (Show)

-- |Arrival at a stop.
data StopArrival = StopArrival
  { stopArrivalTime :: LocalTime
  , stopArrivalDelayMins :: Integer
  } deriving (Show)

-- |Departure from a stop.
data StopDeparture = StopDeparture
  { stopDepartureTime :: LocalTime
  , stopDepartureDelayMins :: Integer
  } deriving (Show)

-- |All possible errors which could occur while fetching data, including HTTP
-- errors and JSON parsing errors.
data Error = HttpResetError | HttpClosedError | HttpParseError String |
             HttpMiscError String | JsonParseError String deriving (Show)

-- |Given information about a desired route, query and return data from DVB.
route :: RouteRequest -> IO RouteResult
route req = do
  let (Location origin) = routeReqOrigin req
      (Location destination) = routeReqDestination req
      (City cityOrigin) = routeReqCityOrigin req
      (City cityDestination) = routeReqCityDestination req
      (year, month, day) = toGregorian $ localDay $ routeReqTime req
      (TimeOfDay hour minute _) = localTimeOfDay $ routeReqTime req
      params = [("sessionID", "0"),
                ("requestID", "0"),
                ("language", "de"),
                ("execInst", "normal"),
                ("command", ""),
                ("ptOptionsActive", "-1"),
                ("itOptionsActive", ""),
                ("itdTripDateTimeDepArr", show $ routeReqTimeType req),
                ("itDateDay", show day),
                ("itDateMonth", show month),
                ("itDateYear", show year),
                ("itdTimeHour", show hour),
                ("idtTimeMinute", show minute),
                ("place_origin", cityOrigin),
                ("placeState_origin", "empty"),
                ("type_origin", "stop"),
                ("name_origin", origin),
                ("nameState_origin", "empty"),
                ("place_destination", cityDestination),
                ("placeState_destination", "empty"),
                ("type_destination", "stop"),
                ("name_destination", destination),
                ("nameState_destination", "empty"),
                ("outputFormat", "JSON"),
                ("coordOutputFormat", "WGS84"),
                ("coordOutputFormatTail", "0")]
  result <- simpleHTTP (getRequest (routeUrl ++ "?" ++ urlEncodeVars params))
  case result of
    Left connError -> return $ Left $ connErrToResultErr connError
    Right response' ->
      let body = BS8.pack $ rspBody response'
      in case eitherDecode body of
        Left err -> return $ Left $ JsonParseError err
        Right route' -> return $ Right (fromJsonRoute route')

-- |The HTTP URL to query for DVB route data.
routeUrl :: String
routeUrl = "http://efa.vvo-online.de:8080/dvb/XML_TRIP_REQUEST2"

-- |Take an HTTP connection error and convert it into an error which can be
-- returned in a RouteResult.
connErrToResultErr :: ConnError -> Error
connErrToResultErr ErrorReset = HttpResetError
connErrToResultErr ErrorClosed = HttpClosedError
connErrToResultErr (ErrorParse msg) = HttpParseError msg
connErrToResultErr (ErrorMisc msg) = HttpMiscError msg

-- |Convert intermediate parsed JSON type to exported route type.
fromJsonRoute :: JSON.Route -> Route
fromJsonRoute route' =
  Route { routeTrips = map fromJsonTrip $ JSON.routeTrips route' }

-- |Convert intermediate parsed JSON type to exported trip type.
fromJsonTrip :: JSON.Trip -> Trip
fromJsonTrip trip' =
  Trip {
      tripDuration = JSON.tripDuration trip',
      tripLegs = map fromJsonLeg $ JSON.tripLegs trip'
    }

-- |Convert intermediate parsed JSON type to exported leg type.
fromJsonLeg :: JSON.Leg -> Leg
fromJsonLeg leg' =
  Leg {
      legNumber = JSON.legModeNumber $ JSON.legMode leg',
      legDesc = JSON.legModeDesc $ JSON.legMode leg',
      legStops = map fromJsonStop $ JSON.legStops leg'
    }

-- |Convert intermediate parsed JSON type to exported stop type.
fromJsonStop :: JSON.Stop -> Stop
fromJsonStop stop' =
  let arrTime = JSON.stopRefArrivalTime $ JSON.stopRef stop'
      arrDelayMins = JSON.stopRefArrivalDelayMins $ JSON.stopRef stop'
      arrival = case (arrTime, arrDelayMins) of
        (Just arrTime', Just arrDelayMins') ->
          let parsedArrTime = parseTime defaultTimeLocale "%Y%m%d %H:%M"
                                        arrTime'
              arrTimeToStopDeparture arrTime'' = StopArrival {
                  stopArrivalTime = arrTime'',
                  stopArrivalDelayMins = properDelay $ read arrDelayMins'
                }
          in arrTimeToStopDeparture <$> parsedArrTime
        _ -> Nothing
      depTime = JSON.stopRefDepartureTime $ JSON.stopRef stop'
      depDelayMins = JSON.stopRefDepartureDelayMins $ JSON.stopRef stop'
      departure = case (depTime, depDelayMins) of
        (Just depTime', Just depDelayMins') ->
          let parsedDepTime = parseTime defaultTimeLocale "%Y%m%d %H:%M"
                                        depTime'
              depTimeToStopDeparture depTime'' = StopDeparture {
                  stopDepartureTime = depTime'',
                  stopDepartureDelayMins = properDelay $ read depDelayMins'
                }
          in depTimeToStopDeparture <$> parsedDepTime
        _ -> Nothing
  in Stop {
      stopName = JSON.stopName stop',
      stopPlatformName = JSON.stopPlatformName stop',
      stopArrival = arrival,
      stopDeparture = departure
    }

-- |Convert the arrival/departure delay that is received from the server to a
-- more meaningful value.
properDelay :: Integer -> Integer
properDelay (-1) = 0
properDelay x = x
