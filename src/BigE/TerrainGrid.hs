-- |
-- Module: BigE.TerrainGrid
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Terrain grids are 2D data structures carrying 3D values representing
-- vertices and squares for a grid. Terrain grids can be used to generate
-- meshes, it can also be used as runtime lookup for terrain height/
-- terrain collision.
module BigE.TerrainGrid
    ( TerrainGrid
    , fromImageMap
    , verticeGridSize
    , squareGridSize
    ) where

import           BigE.ImageMap (ImageElement (..), ImageMap, PixelRGB8 (..),
                                elementAt, imageSize)
import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector
import           Graphics.GL   (GLfloat)
import           Linear        (V3 (..))

-- | The terrain grid. Upper left corner of the grid is always at 0, 0. To
-- move the grid elsewhere require translation.
newtype TerrainGrid = TerrainGrid (Vector Row)
    deriving Show

type Row = Vector (V3 GLfloat)

-- | Create a 'TerrainGrid' from the height scaling factor and the 'ImageMap'.
-- All heights in the 'ImageMap' are divided by the scaling factor.
fromImageMap :: Float -> ImageMap -> Either String TerrainGrid
fromImageMap heightScale imageMap
    | bigEnough (imageSize imageMap) =
        let (width, height) = imageSize imageMap
            gridVector = Vector.generate height $ mkRow heightScale imageMap width
        in Right (TerrainGrid gridVector)

    | otherwise = Left "ImageMap must be at least (2, 2) in size"
    where
        bigEnough :: (Int, Int) -> Bool
        bigEnough (w, h) = w >= 2 && h >= 2

-- | Get the size of the vertice grid. I.e. the grid of individual points
-- in (width, height).
verticeGridSize :: TerrainGrid -> (Int, Int)
verticeGridSize (TerrainGrid gridVector)
    | Vector.null gridVector = (0, 0) -- Should not happen
    | otherwise = (Vector.length $ Vector.head gridVector, Vector.length gridVector)

-- | Get the size of the square grid. I.e. the grid of squares. It is the
-- the vertice grid -1.
squareGridSize :: TerrainGrid -> (Int, Int)
squareGridSize terrainGrid =
    let (w, h) = verticeGridSize terrainGrid
    in (w - 1, h - 1)

-- | Make a single row.
mkRow :: Float -> ImageMap -> Int -> Int -> Row
mkRow heightScale imageMap width row =
    Vector.generate width $ \col ->
        let pixel = elementAt row col imageMap
            x = fromIntegral col
            y = fromIntegral row
            z = convertPixel pixel / heightScale
        in V3 x y z
    where
        convertPixel :: ImageElement -> GLfloat
        convertPixel (Raw val)               = fromIntegral val
        convertPixel (RGB (PixelRGB8 r _ _)) = fromIntegral r -- Assume grey
