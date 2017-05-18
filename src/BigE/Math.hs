-- |
-- Module: BigE.Math
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Math
    ( toRadians
    , toDegrees
    ) where

-- | Convert degrees to radians.
toRadians :: Floating a => a -> a
toRadians deg = deg * (pi / 180)

-- | Convert radians to degrees.
toDegrees :: Floating a => a -> a
toDegrees rad = rad / (pi / 180)
