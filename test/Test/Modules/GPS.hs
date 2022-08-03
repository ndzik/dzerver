{-# LANGUAGE TypeApplications #-}

module Test.Modules.GPS where

import Data.AEq
import Dzerver.Modules.GPS
import Test.QuickCheck hiding ((===))

instance Arbitrary Location where
  -- TODO: Separate DD from DMS on typelevel?
  arbitrary = do
    scale <- arbitrary @Int
    DD . (*) (fromIntegral scale) <$> arbitrary

instance AEq Location where
  DD a === DD b = a === b
  DMS ad am as === DMS bd bm bs = ad === bd && am === bm && as === bs
  a@(DD _) === b = a === convert b
  a === b@(DD _) = convert a === b

  DD a ~== DD b = a ~== b
  DMS ad am as ~== DMS bd bm bs = ad ~== bd && am ~== bm && as ~== bs
  a@(DD _) ~== b = a ~== convert b
  a ~== b@(DD _) = convert a ~== b

prop_Convert :: Location -> Bool
prop_Convert l = l ~== (convert . convert $ l)
