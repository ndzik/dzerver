{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Dzerver.Modules.GPS
  ( LocationUnit (..),
    Latitude (..),
    Longitude (..),
    Position (..),
    convert,
  )
where

import Data.Aeson
import GHC.Float
import GHC.Generics

data Position = Position Latitude Longitude
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

newtype Longitude = Longitude LocationUnit
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

newtype Latitude = Latitude LocationUnit
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data LocationUnit
  = -- | Decimal Degrees
    DD Double
  | -- | Degrees Minutes Seconds
    DMS Double Double Double
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

convert :: LocationUnit -> LocationUnit
convert (DD deg) =
  let deg' = truncateDouble deg
      m = (deg - deg') * 60
      m' = truncateDouble m
      s' = (m - m') * 60
   in DMS deg' m' s'
convert (DMS deg m s) =
  let deg' = deg + (m / 60) + (s / (60 * 60))
   in DD deg'

truncateDouble :: Double -> Double
truncateDouble = int2Double . truncate
