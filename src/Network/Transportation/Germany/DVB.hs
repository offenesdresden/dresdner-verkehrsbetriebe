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
, defaultCity
) where

newtype Location = Location String deriving (Show)
newtype City = City String deriving (Show)

data TimeType = DepartureTime | ArrivalTime

instance Show TimeType where
  show DepartureTime = "dep"
  show ArrivalTime = "arr"

defaultCity :: City
defaultCity = City "Dresden"
