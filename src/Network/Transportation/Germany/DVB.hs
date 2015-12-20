-- |
-- Module: Network.Transportation.Germany.DVB
-- Copyright: (C) 2015 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Network.Transportation.Germany.DVB
( Location(..)
, City(..)
, TimeType(..)
, RouteRequest(..)
, route
) where

import Network.HTTP
import Network.Stream
import Data.Time.Calendar
import Data.Time.LocalTime

newtype Location = Location String
newtype City = City String
data TimeType = DepartureTime | ArrivalTime

instance Show TimeType where
  show DepartureTime = "dep"
  show ArrivalTime = "arr"

data RouteRequest = RouteRequest
  { routeReqOrigin :: Location
  , routeReqDestination :: Location
  , routeReqCityOrigin :: City
  , routeReqCityDestination :: City
  , routeReqTime :: LocalTime
  , routeReqTimeType :: TimeType
  }

defaultCity :: City
defaultCity = City "Dresden"

route :: RouteRequest -> IO (Either ConnError String)
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
    Left connError -> return $ Left connError
    Right response -> return $ Right $ rspBody response

routeUrl :: String
routeUrl = "http://efa.vvo-online.de:8080/dvb/XML_TRIP_REQUEST2"
