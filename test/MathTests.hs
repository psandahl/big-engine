module MathTests
    ( toRadiansConversion
    , toDegreesConversion
    , heightInFlatTriangle
    , heightInZSkewedTriangle
    , heightInXSkewedTriangle
    , frontFacingNormals
    , differentFacingNormals
    ) where

import           Test.HUnit
import           Test.HUnit.Approx

import           BigE.Math         (baryCentricHeight, surfaceNormal, toDegrees,
                                    toRadians)
import           Linear            (V3 (..), normalize)

-- | Check conversion to radians.
toRadiansConversion :: Assertion
toRadiansConversion = do
    0 `equalTo` toRadians 0
    (pi / 2) `equalTo` toRadians 90
    pi `equalTo` toRadians 180
    (pi * 2) `equalTo` toRadians 360

-- | Check conversion to degrees,
toDegreesConversion :: Assertion
toDegreesConversion = do
    0 `equalTo` toDegrees 0
    90 `equalTo` toDegrees (pi / 2)
    180 `equalTo` toDegrees pi
    360 `equalTo` toDegrees (pi * 2)

-- | Height calculation in a perfectly flat triangle with the height of zero
-- over the whole area.
heightInFlatTriangle :: Assertion
heightInFlatTriangle = do
    let p1 = V3 0 0 0
        p2 = V3 1 0 0
        p3 = V3 0 0 1
    0 `equalTo` baryCentricHeight p1 p2 p3 0 0
    0 `equalTo` baryCentricHeight p1 p2 p3 1 0
    0 `equalTo` baryCentricHeight p1 p2 p3 0 1
    0 `equalTo` baryCentricHeight p1 p2 p3 0.5 0.25

-- | Height calculation in a triangle skewed on the Z-axis. Lowest -0.5 and
-- highest 0.5.
heightInZSkewedTriangle :: Assertion
heightInZSkewedTriangle = do
    let p1 = V3 0 (-0.5) 0
        p2 = V3 1 0.5 0
        p3 = V3 0 (-0.5) 1
    (-0.5) `equalTo` baryCentricHeight p1 p2 p3 0 0
    0.5 `equalTo` baryCentricHeight p1 p2 p3 1 0
    (-0.5) `equalTo` baryCentricHeight p1 p2 p3 0 1
    (-0.25) `equalTo` baryCentricHeight p1 p2 p3 0.25 0.25
    0 `equalTo` baryCentricHeight p1 p2 p3 0.5 0.25
    0.25 `equalTo` baryCentricHeight p1 p2 p3 0.75 0.25

-- | Height calculation in a triangle skewed on the X-axis. Lowest -0.5 and
-- highest 0.5.
heightInXSkewedTriangle :: Assertion
heightInXSkewedTriangle = do
    let p1 = V3 0 (-0.5) 0
        p2 = V3 1 (-0.5) 0
        p3 = V3 0 0.5 1
    (-0.5) `equalTo` baryCentricHeight p1 p2 p3 0 0
    (-0.5) `equalTo` baryCentricHeight p1 p2 p3 1 0
    0.5 `equalTo` baryCentricHeight p1 p2 p3 0 1
    (-0.25) `equalTo` baryCentricHeight p1 p2 p3 0.25 0.25
    0 `equalTo` baryCentricHeight p1 p2 p3 0.25 0.5
    0.25 `equalTo` baryCentricHeight p1 p2 p3 0.25 0.75

-- | A quad made of two triangles. The surface normal for the two triangles
-- shall point in the same direction - in the positive z direction.
frontFacingNormals :: Assertion
frontFacingNormals = do
    front @=? surfaceNormal p1 p2 p3
    front @=? surfaceNormal p1 p3 p4
    where
        p1, p2, p3, p4, front :: V3 Float
        p1 = V3 1 1 0
        p2 = V3 0 1 0
        p3 = V3 0 0 0
        p4 = V3 1 0 0
        front = V3 0 0 1

-- | Two triangles - but with shared vertices - having their surfaces pointing
-- away from each other by 90 degrees.
differentFacingNormals :: Assertion
differentFacingNormals = do
    leftNormal @=? surfaceNormal p1 p2 p3
    rightNormal @=? surfaceNormal p1 p3 p4
    where
        p1, p2, p3, p4, leftNormal, rightNormal :: V3 Float
        p1 = V3 0 0.5 0
        p2 = V3 (-vertOffset) 0 (-vertOffset)
        p3 = V3 0 (-0.5) 0
        p4 = V3 vertOffset 0 (-vertOffset)
        leftNormal = normalize $ V3 (-1) 0 1
        rightNormal = normalize $ V3 1 0 1

        -- Vertice offset for x and z for a point moved back 45 degrees.
        vertOffset :: Float
        vertOffset = sin (toRadians 45)

equalTo :: Float -> Float -> Assertion
equalTo = assertApproxEqual "Shall be equal" closeEnough

closeEnough :: Float
closeEnough = 0.00001
