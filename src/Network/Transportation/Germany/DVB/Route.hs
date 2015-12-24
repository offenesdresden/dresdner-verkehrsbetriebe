-- |
-- Module: Network.Transportation.Germany.DVB.Route
-- Copyright: (C) 2015 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Transportation.Germany.DVB.Route
( RouteRequest(..)
, Leg(..)
, Trip(..)
, Route(..)
, route
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Functor
import qualified Data.HashMap.Strict as HMS
import Data.Time.Calendar
import Data.Time.LocalTime
import qualified Data.Vector as V
import Network.HTTP
import Network.Stream
import Network.Transportation.Germany.DVB

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

instance FromJSON Route where
  parseJSON (Object obj) = Route <$>
                           obj .: "trips"
  parseJSON _ = mzero

-- |One of the possible trips to get from the origin to the destination.
data Trip = Trip
  { tripDuration :: String
  , tripLegs :: [Leg]
  } deriving (Show)

instance FromJSON Trip where
  parseJSON (Object obj) = Trip <$>
                           obj .: "duration" <*>
                           obj .: "legs"
  parseJSON _ = mzero

-- |One segment of a trip (for example: one vehicle).
data Leg = Leg
  { legNumber :: String
  , legDesc :: String
  } deriving (Show)

instance FromJSON Leg where
  parseJSON (Object obj) =
    let parseMode :: Value -> Parser Leg
        parseMode (Object modeObj) = Leg <$>
                                     modeObj .: "number" <*>
                                     modeObj .: "desc"
        parseMode _ = mzero
    in case HMS.lookup "mode" obj of
      Just modeVal -> parseMode modeVal
      Nothing -> mzero
  parseJSON _ = mzero

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
                ("language", "en"),
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
        Right route -> return $ Right route

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
