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
    , aspectRatio
    , mkPerspective
    , mkTranslate
    , mkScale
    , mkRotateTranslate
    , mkRotate
    ) where

import           Linear (Epsilon, M44, V3 (..), V4 (..), axisAngle,
                         mkTransformation, perspective, zero)

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

-- | Calculate the aspect ratio of a widht and a height.
aspectRatio :: Floating a => (Int, Int) -> a
aspectRatio (width, height) = fromIntegral width / fromIntegral height

-- | Utility function to setup a perspective matrix with the aspect ratio
-- given by the dimensions, a field of view of 45 degrees and a
-- view from 0.001 to 1000.
mkPerspective :: Floating a => (Int, Int) -> M44 a
mkPerspective dimensions =
    perspective (toRadians 45) (aspectRatio dimensions) 0.001 1000

-- | Utility function to setup a translation matrix.
mkTranslate :: Floating a => V3 a -> M44 a
mkTranslate (V3 x y z) =
    V4 (V4 1 0 0 x)
       (V4 0 1 0 y)
       (V4 0 0 1 z)
       (V4 0 0 0 1)

-- | Utility function to setup a scaling matrix.
mkScale :: Floating a => V3 a -> M44 a
mkScale (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)

-- | Utility function to setup a combined rotation and translation matrix. The
-- angle is expressed in radians.
mkRotateTranslate :: (Epsilon a, Floating a) => V3 a -> a -> V3 a -> M44 a
mkRotateTranslate axis theta = mkTransformation (axisAngle axis theta)

-- | Utility function to setup a rotation matrix.
mkRotate :: (Epsilon a, Floating a) => V3 a -> a -> M44 a
mkRotate axis theta = mkRotateTranslate axis theta zero
