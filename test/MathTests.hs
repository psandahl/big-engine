module MathTests
    ( toRadiansConversion
    , toDegreesConversion
    ) where

import           Test.HUnit
import           Test.HUnit.Approx

import           BigE.Math         (toDegrees, toRadians)

toRadiansConversion :: Assertion
toRadiansConversion = do
    0 `equalTo` toRadians 0
    (pi / 2) `equalTo` toRadians 90
    pi `equalTo` toRadians 180
    (pi * 2) `equalTo` toRadians 360

toDegreesConversion :: Assertion
toDegreesConversion = do
    0 `equalTo` toDegrees 0
    90 `equalTo` toDegrees (pi / 2)
    180 `equalTo` toDegrees pi
    360 `equalTo` toDegrees (pi * 2)

equalTo :: Float -> Float -> Assertion
equalTo expected = assertApproxEqual "Shall be equal" closeEnough expected

closeEnough :: Float
closeEnough = 0.00001
