-- |
-- Module: BigE.Math
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Common math utility functions.
module BigE.Math
    ( toRadians
    , toDegrees
    , baryCentricHeight
    ) where

import           Linear (V3 (..))

-- | Convert degrees to radians.
toRadians :: Floating a => a -> a
toRadians deg = deg * (pi / 180)

-- | Convert radians to degrees.
toDegrees :: Floating a => a -> a
toDegrees rad = rad / (pi / 180)

-- | Calculate the the height value at the point x, z with the help of
-- the triangle given by the vertices. For some more theory see:
-- <https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Conversion_between_barycentric_and_Cartesian_coordinates>
baryCentricHeight :: (Fractional a, Num a)
                  => V3 a -> V3 a -> V3 a -> a -> a -> a
baryCentricHeight (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) x z =
    let det = (z2 - z3) * (x1 - x3) + (x3 - x2) * (z1 - z3)
        l1 = ((z2 - z3) * (x - x3) + (x3 - x2) * (z - z3)) / det
        l2 = ((z3 - z1) * (x - x3) + (x1 - x3) * (z - z3)) / det
        l3 = 1.0 - l1 - l2
    in l1 * y1 + l2 * y2 + l3 * y3
