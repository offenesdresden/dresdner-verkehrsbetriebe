-- |
-- Module: Network.Transportation.Germany.DVB.Monitor.JSON
-- Copyright: (C) 2016 Braden Walters
-- License: MIT (see LICENSE file)
-- Maintainer: Braden Walters <vc@braden-walters.info>
-- Stability: experimental
-- Portability: ghc

{-# LANGUAGE OverloadedStrings #-}

module Network.Transportation.Germany.DVB.Monitor.JSON
( TransitConnection(..)
) where

import Data.Aeson
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector as V

-- |An arriving vehicle at the stop.
data TransitConnection = TransitConnection
  { transConnNumber :: String
  , transConnDesc :: String
  , transConnArrivalMinutes :: String
  } deriving (Show)

instance FromJSON TransitConnection where
  parseJSON (Array arr) =
    if V.length arr == 3 then
      case (arr V.! 0, arr V.! 1, arr V.! 2) of
        (String number, String desc, String arrivalMinutes) ->
          return $ TransitConnection (T.unpack number) (T.unpack desc)
                                     (T.unpack arrivalMinutes)
        _ -> mzero
    else mzero
  parseJSON _ = mzero
