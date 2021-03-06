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
    , StorableVector
    , fromImageMap
    , verticeGridSize
    , quadGridSize
    , lookup
    , terrainHeight
    , asVertP
    , asVertPNTxC
    , indexVector
    ) where

import qualified BigE.Attribute.Vert_P        as Vert_P
import qualified BigE.Attribute.Vert_P_N_Tx_C as Vert_P_N_Tx_C
import           BigE.ImageMap                (ImageElement (..), ImageMap,
                                               PixelRGB8 (..), elementAt,
                                               imageSize, toRGBA)
import           BigE.Math                    (baryCentricHeight, surfaceNormal)
import           Control.Monad.ST             (runST)
import           Control.Monad.Primitive      (PrimMonad, PrimState)
import           Data.Vector                  (Vector, (!))
import qualified Data.Vector                  as Vector
import qualified Data.Vector.Storable         as SVector
import Data.Vector.Storable.Mutable          (MVector)
import qualified Data.Vector.Storable.Mutable as SMVector
import           Graphics.GL                  (GLfloat, GLuint)
import           Linear                       (V2 (..), V3 (..), normalize)
import           Prelude                      hiding (lookup)

-- | The terrain grid. Upper left corner of the grid is always at 0, 0. To
-- move the grid elsewhere require translation.
data TerrainGrid = TerrainGrid
    { gridVector :: !(Vector (V3 GLfloat))
      -- ^ The storage of vertice 3D vectors.

    , gridWidth  :: !Int
      -- ^ The width of the grid, in vertices.

    , gridHeight :: !Int
      -- ^ The height of the grid, in vertices.
    } deriving Show

-- | Type alias for a storable vector.
type StorableVector = SVector.Vector

-- | Create a 'TerrainGrid' from the height scaling factor and the 'ImageMap'.
-- All heights in the 'ImageMap' are divided by the scaling factor.
fromImageMap :: Float -> ImageMap -> Either String TerrainGrid
fromImageMap heightScale imageMap
    | bigEnough (imageSize imageMap) =
        let (width, height) = imageSize imageMap
            gridLength = width * height
        in Right TerrainGrid
            { gridVector = Vector.generate gridLength $ \index ->
                let x = index `mod` width
                    z = index `div` width
                    y = convertPixel $ elementAt x z imageMap
                in V3 (fromIntegral x) (y / heightScale) (fromIntegral z)
            , gridWidth = width
            , gridHeight = height
            }

    | otherwise = Left "ImageMap must be at least (2, 2) in size"
    where
        bigEnough :: (Int, Int) -> Bool
        bigEnough (w, h) = w >= 2 && h >= 2

        convertPixel :: ImageElement -> GLfloat
        convertPixel (Raw val)               = fromIntegral val
        convertPixel (RGB (PixelRGB8 r _ _)) = fromIntegral r -- Assume grey

-- | Get the size of the vertice grid. I.e. the grid of individual points
-- in (width, height).
verticeGridSize :: TerrainGrid -> (Int, Int)
verticeGridSize terrainGrid = (gridWidth terrainGrid, gridHeight terrainGrid)

-- | Get the size of the quad grid. I.e. the grid of quads. It is the
-- the vertice grid -1 on each component.
quadGridSize :: TerrainGrid -> (Int, Int)
quadGridSize terrainGrid =
    let (w, h) = verticeGridSize terrainGrid
    in (w - 1, h - 1)

-- Get the value at vertice x, z in the grid. Lookup is based on 'Vectors'
-- unsafe lookup, so beware ...
lookup :: (Int, Int) -> TerrainGrid -> V3 GLfloat
lookup (x, z) terrainGrid =
    let index = z * gridWidth terrainGrid + x
    in gridVector terrainGrid ! index

-- | Calculate the height - y - under the 2D position given by x and z. The
-- grid's origin is always at x = 0, z = 0. If the given position is outside
-- of the grid the height of zero is returned. If the grid not is placed at
-- 0, 0 in model space it is up to the application to do that mapping.
terrainHeight :: (Float, Float) -> TerrainGrid -> GLfloat
terrainHeight (x, z) terrainGrid =
    let (baseX, fracX) = splitFloat x
        (baseZ, fracZ) = splitFloat z
        (width, height) = quadGridSize terrainGrid
    in if baseX >= 0 && baseX < width && baseZ >= 0 && baseZ < height
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

-- | Export the 'TerrainGrid' as a vector of 'Vert_P.Vertex' and a
-- corresponding index vector.
asVertP :: TerrainGrid -> (StorableVector Vert_P.Vertex, StorableVector GLuint)
asVertP terrainGrid =
    let gridVector' = gridVector terrainGrid
        verts = SVector.generate (Vector.length gridVector') $ \index ->
            Vert_P.Vertex { Vert_P.position = gridVector' ! index }
        indices = indexVector terrainGrid
    in (verts, indices)

-- | Export the 'TerrainGrid' as a vector of 'Vert_P_N_Tx_C.Vertex' and a
-- corresponding index vector. The resulting vertices will be colored according
-- to the 'ImageMap', they will have texture coordinates and generated normals
-- for smooth shading. If the sizes of the 'ImageMap' and the 'TerrainGrid'
-- does not match the export will fail.
asVertPNTxC :: ImageMap -> TerrainGrid
            -> Either String ( StorableVector Vert_P_N_Tx_C.Vertex
                             , StorableVector GLuint
                             )
asVertPNTxC imageMap terrainGrid
    | imageSize imageMap == verticeGridSize terrainGrid =
        let gridVector' = gridVector terrainGrid
            width = gridWidth terrainGrid
            height = gridHeight terrainGrid

            -- First phase. Set position, texCoord and color. Leave normal with a
            -- zero vector.
            verts = SVector.generate (Vector.length gridVector') $ \index ->
                let row = index `div` width
                    col = index `mod` width
                    t = height - row - 1
                in Vert_P_N_Tx_C.Vertex
                       { Vert_P_N_Tx_C.position = gridVector' ! index
                       , Vert_P_N_Tx_C.normal = V3 0 0 0
                       , Vert_P_N_Tx_C.texCoord = V2 (fromIntegral col) (fromIntegral t)
                       , Vert_P_N_Tx_C.color = toRGBA $ elementAt row col imageMap
                       }

            -- Make indices.
            indices = indexVector terrainGrid

            -- Calulate smoothed vertex normals
            verts' = SVector.map normalizeVertex $ setVertexNormals verts indices

            -- Done!
        in Right (verts', indices)
    | otherwise = Left "Dimensions must match"
    where
        normalizeVertex :: Vert_P_N_Tx_C.Vertex -> Vert_P_N_Tx_C.Vertex
        normalizeVertex vert =
            vert { Vert_P_N_Tx_C.normal = normalize $ Vert_P_N_Tx_C.normal vert }

-- | Calculate smooth vertex normals from surface normals.
setVertexNormals :: StorableVector Vert_P_N_Tx_C.Vertex
                  -> StorableVector GLuint
                  -> StorableVector Vert_P_N_Tx_C.Vertex
setVertexNormals verts indices = runST $ do
    mutableVerts <- SVector.unsafeThaw verts
    setSurfaceNormal 0 (SVector.length indices) indices mutableVerts
    SVector.unsafeFreeze mutableVerts

-- | Iterate through the mutable array, calculate the surface normal and
-- add the surface normal to the vertices for the surface of each triangle. The
-- triangle's vertices are accessed through the index array.
-- It will result in a smoothed normal from all surfaces sharing a vertex.
setSurfaceNormal :: PrimMonad m => Int -> Int -> StorableVector GLuint
                 -> MVector (PrimState m) Vert_P_N_Tx_C.Vertex -> m ()
setSurfaceNormal metaIndex numIndices indices vec
    | metaIndex < numIndices = do
        let i0 = fromIntegral $ SVector.unsafeIndex indices metaIndex
            i1 = fromIntegral $ SVector.unsafeIndex indices (metaIndex + 1)
            i2 = fromIntegral $ SVector.unsafeIndex indices (metaIndex + 2)

        v0 <- SMVector.read vec i0
        v1 <- SMVector.read vec i1
        v2 <- SMVector.read vec i2

        -- Calculate the normal for the surface.
        let norm = surfaceNormal (Vert_P_N_Tx_C.position v0)
                                 (Vert_P_N_Tx_C.position v1)
                                 (Vert_P_N_Tx_C.position v2)

        -- Update the vertices.
        SMVector.write vec i0 $ v0 { Vert_P_N_Tx_C.normal = Vert_P_N_Tx_C.normal v0 + norm }
        SMVector.write vec i1 $ v1 { Vert_P_N_Tx_C.normal = Vert_P_N_Tx_C.normal v1 + norm }
        SMVector.write vec i2 $ v2 { Vert_P_N_Tx_C.normal = Vert_P_N_Tx_C.normal v2 + norm }

        -- Iterate to the meta index for the next triangle.
        setSurfaceNormal (metaIndex + 3) numIndices indices vec

    -- The interation is done.
    | otherwise = return ()

-- | Generate a 'StorableVector' of indices. Usable for creating meshes.
indexVector :: TerrainGrid -> StorableVector GLuint
indexVector terrainGrid =
    let (quadWidth, quadHeight) = quadGridSize terrainGrid
        numIndices = quadWidth * quadHeight * 6
        vertexWidth = quadWidth + 1
    in SVector.generate numIndices $ \index ->
        let quadIndex = index `div` 6
            rowOffset = index `div` (quadWidth * 6)
            baseIndex = quadIndex + rowOffset
            vertexIndex = index `mod` 6
        in gridIndex baseIndex vertexWidth vertexIndex

-- | Split a Float into a tuple with the base, Int, part and the fractional part.
splitFloat :: Float -> (Int, Float)
splitFloat value =
    let base = floor value
        frac = value - fromIntegral base
    in (base, frac)

-- | Each quad in the grid is made up of two triangles, and six indices are
-- needed to draw a quad. Given a base index for the quad, a vertex width and
-- the particular vertex index wanted a value can be produced.
gridIndex :: Int -> Int -> Int -> GLuint
gridIndex baseIndex vertexWidth vertexIndex
    | vertexIndex == 0 = fromIntegral $ baseIndex + 1
    | vertexIndex == 1 = fromIntegral baseIndex
    | vertexIndex == 2 = fromIntegral $ baseIndex + vertexWidth
    | vertexIndex == 3 = fromIntegral $ baseIndex + 1
    | vertexIndex == 4 = fromIntegral $ baseIndex + vertexWidth
    | vertexIndex == 5 = fromIntegral $ baseIndex + vertexWidth + 1
    | otherwise = error "Index can only range: [0 - 5]"
