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
    , lookup
    , terrainHeight
    ) where

import           BigE.ImageMap (ImageElement (..), ImageMap, PixelRGB8 (..),
                                elementAt, imageSize)
import           BigE.Math     (baryCentricHeight)
import           Data.Vector   (Vector, (!))
import qualified Data.Vector   as Vector
import           Graphics.GL   (GLfloat)
import           Linear        (V3 (..))
import           Prelude       hiding (lookup)

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

-- Get the value at point x, z in the grid. Lookup is based on 'Vectors'
-- unsafe lookup, so beware ...
lookup :: (Int, Int) -> TerrainGrid -> V3 GLfloat
lookup (x, z) (TerrainGrid gridVector)=
    let row = gridVector ! z
    in row ! x

-- | Calculate the height - y - under the 2D position given by x and z. The
-- grid's origin is always at x = 0, z = 0. If the given position is outside
-- of the grid the height of zero is returned.
terrainHeight :: (Float, Float) -> TerrainGrid -> GLfloat
terrainHeight (x, z) terrainGrid =
    let (baseX, fracX) = splitFloat x
        (baseZ, fracZ) = splitFloat z
        (width, height) = verticeGridSize terrainGrid
    in if baseX < width && baseZ < height
           then
               let (p1, p2, p3) = triSelect baseX baseZ $ fracX + fracZ
               in baryCentricHeight p1 p2 p3 x z
           else 0
    where
        -- Selection of the triangle the point is inside. If the selection
        -- value is greater than 1 then the point is inside the right triangle.
        -- x' and z' both represent the top left vertice of the square
        -- containing the point.
        triSelect :: Int -> Int -> Float -> (V3 GLfloat, V3 GLfloat, V3 GLfloat)
        triSelect x' z' selection
            | selection > 1.0 =
                let p1 = lookup (x' + 1, z') terrainGrid
                    p2 = lookup (x', z') terrainGrid
                    p3 = lookup (x' + 1, z' + 1) terrainGrid
                in (p1, p2, p3)
            | otherwise =
                let p1 = lookup (x', z') terrainGrid
                    p2 = lookup (x' + 1, z') terrainGrid
                    p3 = lookup (x', z' + 1) terrainGrid
                in (p1, p2, p3)

-- | Make a single row.
mkRow :: Float -> ImageMap -> Int -> Int -> Row
mkRow heightScale imageMap width row =
    Vector.generate width $ \col ->
        let pixel = elementAt col row imageMap
            x = fromIntegral col
            y = convertPixel pixel / heightScale
            z = fromIntegral row
        in V3 x y z
    where
        convertPixel :: ImageElement -> GLfloat
        convertPixel (Raw val)               = fromIntegral val
        convertPixel (RGB (PixelRGB8 r _ _)) = fromIntegral r -- Assume grey

splitFloat :: Float -> (Int, Float)
splitFloat value =
    let base = floor value
        frac = value - fromIntegral base
    in (base, frac)
