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

import Network.HTTP
import Network.Transportation.Germany.DVB

-- |All data sent to DVB to monitor a stop.
data MonitorRequest = MonitorRequest
  { monitorReqCity :: City
  , monitorReqStop :: Location
  , monitorReqOffset :: Integer
  , monitorReqLimit :: Integer
  } deriving (Show)

-- |Either monitor data or an error.
type MonitorResult = String

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
    Left connError -> return $ show connError
    Right response' -> return $ rspBody response'

-- |The HTTP URL to query for DVB monitor data.
monitorUrl :: String
monitorUrl = "http://widgets.vvo-online.de/abfahrtsmonitor/Abfahrten.do"
