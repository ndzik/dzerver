module Dzerver.Modules.GPS
  ( Location (..),
    convert,
  )
where

import GHC.Float

data Location
  = -- | Decimal Degrees
    DD Double
  | -- | Degrees Minutes Seconds
    DMS Double Double Double
  deriving (Show, Eq)

convert :: Location -> Location
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
