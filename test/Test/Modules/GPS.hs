{-# LANGUAGE TypeApplications #-}

module Test.Modules.GPS where

import Data.AEq
import Dzerver.Modules.GPS
import Test.QuickCheck hiding (scale, (===))

instance Arbitrary LocationUnit where
  arbitrary = do
    scale <- arbitrary @Int
    DD . (*) (fromIntegral scale) <$> arbitrary

instance AEq LocationUnit where
  DD a === DD b = a === b
  DMS ad am as === DMS bd bm bs = ad === bd && am === bm && as === bs
  a@(DD _) === b = a === convert b
  a === b@(DD _) = convert a === b

  DD a ~== DD b = a ~== b
  DMS ad am as ~== DMS bd bm bs = ad ~== bd && am ~== bm && as ~== bs
  a@(DD _) ~== b = a ~== convert b
  a ~== b@(DD _) = convert a ~== b

prop_Convert :: LocationUnit -> Bool
prop_Convert l = l ~== (convert . convert $ l)
