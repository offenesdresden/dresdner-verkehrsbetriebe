-- |
-- Module: Network.Transportation.Germany.DVB.Monitor
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

module Network.Transportation.Germany.DVB.Monitor
( MonitorRequest(..)
, monitor
) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS8
import Network.HTTP
import Network.Stream
import Network.Transportation.Germany.DVB
import qualified Network.Transportation.Germany.DVB.Monitor.JSON as JSON

-- |All data sent to DVB to monitor a stop.
data MonitorRequest = MonitorRequest
  { monitorReqCity :: City
  , monitorReqStop :: Location
  , monitorReqOffset :: Integer
  , monitorReqLimit :: Integer
  } deriving (Show)

-- |Either monitor data or an error.
type MonitorResult = Either Error [TransitConnection]

-- |An arriving vehicle at the stop.
data TransitConnection = TransitConnection
  { transConnNumber :: String
  , transConnDesc :: String
  , transConnArrivalMinutes :: Integer
  } deriving (Show)

-- |All possible errors which could occur while fetching data, including HTTP
-- errors and JSON parsing errors.
data Error = HttpResetError | HttpClosedError | HttpParseError String |
             HttpMiscError String | JsonParseError String deriving (Show)

-- |Given information about a stop, query and return data from DVB about
-- vehicles stopping there.
monitor :: MonitorRequest -> IO MonitorResult
monitor req = do
  let (City city) = monitorReqCity req
      (Location stop) = monitorReqStop req
      params = [("ort", city),
                ("hst", stop),
                ("vz", show $ monitorReqOffset req),
                ("lim", show $ monitorReqLimit req)]
  result <- simpleHTTP (getRequest (monitorUrl ++ "?" ++ urlEncodeVars params))
  case result of
    Left connError -> return $ Left $ connErrToResultErr connError
    Right response' ->
      let body = BS8.pack $ rspBody response'
      in case eitherDecode body of
        Left err ->
          return $ Left $ JsonParseError err
        Right transConns ->
          return $ Right (map fromJsonTransitConnection transConns)

-- |The HTTP URL to query for DVB monitor data.
monitorUrl :: String
monitorUrl = "http://widgets.vvo-online.de/abfahrtsmonitor/Abfahrten.do"

-- |Take an HTTP connection error and convert it into an error which can be
-- returned in a RouteResult.
connErrToResultErr :: ConnError -> Error
connErrToResultErr ErrorReset = HttpResetError
connErrToResultErr ErrorClosed = HttpClosedError
connErrToResultErr (ErrorParse msg) = HttpParseError msg
connErrToResultErr (ErrorMisc msg) = HttpMiscError msg

fromJsonTransitConnection :: JSON.TransitConnection -> TransitConnection
fromJsonTransitConnection transConn =
  TransitConnection {
      transConnNumber = JSON.transConnNumber transConn,
      transConnDesc = JSON.transConnDesc transConn,
      -- TODO: Use a total function instead of read.
      transConnArrivalMinutes = read $ JSON.transConnArrivalMinutes transConn
    }
