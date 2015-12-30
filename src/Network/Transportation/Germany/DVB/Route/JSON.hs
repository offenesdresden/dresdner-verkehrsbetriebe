-- |
-- Module: Network.Transportation.Germany.DVB.Route.JSON
-- Copyright: (C) 2015 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Transportation.Germany.DVB.Route.JSON
( Route(..)
, Trip(..)
, Leg(..)
, LegMode(..)
, Stop(..)
, StopRef(..)
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HMS

-- |Parsing JSON type for routes.
data Route = Route { routeTrips :: [Trip] }

instance FromJSON Route where
  parseJSON (Object obj) = Route <$>
                           obj .: "trips"
  parseJSON _ = mzero

-- |Parsing JSON type for trips.
data Trip = Trip
  { tripDuration :: String
  , tripLegs :: [Leg]
  }

instance FromJSON Trip where
  parseJSON (Object obj) = Trip <$>
                           obj .: "duration" <*>
                           obj .: "legs"
  parseJSON _ = mzero

-- |Parsing JSON type for trip legs.
data Leg = Leg
  { legMode :: LegMode
  , legStops :: [Stop]
  }

instance FromJSON Leg where
  parseJSON (Object obj) = Leg <$>
                           maybe mzero parseJSON (HMS.lookup "mode" obj) <*>
                           (F.concat <$> (obj .:? "stopSeq"))
  parseJSON _ = mzero

-- |Parsing JSON type for the mode object in trip legs.
data LegMode = LegMode
  { legModeNumber :: String
  , legModeDesc :: String
  }

instance FromJSON LegMode where
  parseJSON (Object obj) = LegMode <$>
                           obj .: "number" <*>
                           obj .: "desc"
  parseJSON _ = mzero

-- |Parsing JSON type for stops in trips.
data Stop = Stop
  { stopName :: String
  , stopPlatformName :: String
  , stopRef :: StopRef
  }

instance FromJSON Stop where
  parseJSON (Object obj) = Stop <$>
                           obj .: "nameWO" <*>
                           obj .: "platformName" <*>
                           maybe mzero parseJSON (HMS.lookup "ref" obj)
  parseJSON _ = mzero

-- |Parsing JSON type for the ref object in stops in trips.
data StopRef = StopRef
  { stopRefDepartureTime :: Maybe String
  , stopRefDelayMins :: Maybe String
  }

instance FromJSON StopRef where
  parseJSON (Object obj) = StopRef <$>
                           obj .:? "depDateTime" <*>
                           obj .:? "depDelay"
  parseJSON _ = mzero
