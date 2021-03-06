module TerrainGridTests
    ( withTooSmallImageMap
    , withMinimumImageMap
    , reportingSize
    , checkingContent
    , indexingOutsideGrid
    , selectingCorrectTriangle
    , indicesFor1x1Quad
    , indicesFor2x1Quad
    , indicesFor2x2Quad
    , exportAsVertP
    , exportAsVertPNTxCFailDimensions
    , exportAsVertPNTxC
    ) where

import           Test.HUnit
import           Test.HUnit.Approx

import qualified BigE.Attribute.Vert_P        as Vert_P
import qualified BigE.Attribute.Vert_P_N_Tx_C as Vert_P_N_Tx_C
import           BigE.ImageMap                (ImageMap, PixelRGB8 (..),
                                               VectorSpec (..), fromVector)
import           BigE.TerrainGrid             (TerrainGrid, asVertP,
                                               asVertPNTxC, fromImageMap,
                                               indexVector, lookup,
                                               quadGridSize, terrainHeight,
                                               verticeGridSize)
import qualified Data.Vector                  as Vector
import qualified Data.Vector.Storable         as SVector
import           Linear                       (V2 (..), V3 (..), V4 (..),
                                               normalize)
import           Prelude                      hiding (lookup)

-- | An input ImageMap must be at least 2, 2 big. In this test case
-- the input is too small and the creation shall fail
withTooSmallImageMap :: Assertion
withTooSmallImageMap = do
    let Right imageMap = fromVector (Raw16Vector (1, 1) $ Vector.fromList [1])
    case fromImageMap 1 imageMap of
        Right _ -> assertBool "Shall fail" False
        Left _  -> assertBool "Shall fail" True

-- | Minimum size of input ImageMap. Creation shall succeed.
withMinimumImageMap :: Assertion
withMinimumImageMap = do
    let Right imageMap = fromVector (Raw16Vector (2, 2) $ Vector.fromList [1, 2, 3, 4])
    case fromImageMap 1 imageMap of
        Right _ -> assertBool "Shall succeed" True
        Left _  -> assertBool "Shall succeed" False

-- | Check that a TerrainGrid is reporting the expected sizes. One size for
-- the vertice grid and a smaller size for the quad grid.
reportingSize :: Assertion
reportingSize = do
    let Right imageMap = fromVector (Raw16Vector (3, 2) $ Vector.fromList [1, 2, 3, 4, 5, 6])
        Right terrainGrid = fromImageMap 1 imageMap
    (3, 2) @=? verticeGridSize terrainGrid
    (2, 1) @=? quadGridSize terrainGrid

-- | Verify that the created TerrainGrid has the expected content when looking
-- at the vertices.
checkingContent :: Assertion
checkingContent = do
    let Right imageMap = fromVector (Raw16Vector (3, 3) $
            Vector.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9])
        Right terrainGrid = fromImageMap 1 imageMap

    -- First row
    V3 0 1 0 @=? lookup (0, 0) terrainGrid
    V3 1 2 0 @=? lookup (1, 0) terrainGrid
    V3 2 3 0 @=? lookup (2, 0) terrainGrid

    -- Second row
    V3 0 4 1 @=? lookup (0, 1) terrainGrid
    V3 1 5 1 @=? lookup (1, 1) terrainGrid
    V3 2 6 1 @=? lookup (2, 1) terrainGrid

    -- Third row
    V3 0 7 2 @=? lookup (0, 2) terrainGrid
    V3 1 8 2 @=? lookup (1, 2) terrainGrid
    V3 2 9 2 @=? lookup (2, 2) terrainGrid

-- | Inside the single square in the grid, the height is 1. Outside the grid
-- the size shall be 0.
indexingOutsideGrid :: Assertion
indexingOutsideGrid = do
    let Right imageMap = fromVector (Raw16Vector (2, 2) $ Vector.fromList [1, 1, 1, 1])
        Right terrainGrid = fromImageMap 1 imageMap
    -- Inside grid.
    1 `equalTo` terrainHeight (0.0, 0.0) terrainGrid
    1 `equalTo` terrainHeight (0.99, 0.0) terrainGrid
    1 `equalTo` terrainHeight (0, 0.99) terrainGrid
    1 `equalTo` terrainHeight (0.99, 0.99) terrainGrid
    1 `equalTo` terrainHeight (0.5, 0.5) terrainGrid

    -- Outside grid.
    0 `equalTo` terrainHeight (1.0, 0.0) terrainGrid
    0 `equalTo` terrainHeight (0.0, 1.0) terrainGrid
    0 `equalTo` terrainHeight (1.0, 1.0) terrainGrid

-- | Depending on the point's position within a square the left or right
-- triangle is selected.
selectingCorrectTriangle :: Assertion
selectingCorrectTriangle = do
    let Right imageMap = fromVector (Raw16Vector (2, 2) $ Vector.fromList [0, 1, 0, 1])
        Right terrainGrid = fromImageMap 1 imageMap
    -- To the left edge. Shall be 0.
    0 `equalTo` terrainHeight (0.0, 0.5) terrainGrid

    -- To the right edge. Shall be close to 0.99.
    0.99 `equalTo` terrainHeight (0.99, 0.5) terrainGrid

-- | Generate indices for a 1x1 quad grid.
indicesFor1x1Quad :: Assertion
indicesFor1x1Quad = do
    let Right imageMap = fromVector (Raw16Vector (2, 2) $ Vector.fromList [0, 0, 0, 0])
        Right terrainGrid = fromImageMap 1 imageMap

    SVector.fromList [1, 0, 2, 1, 2, 3] @=? indexVector terrainGrid

-- | Generate indices for a 2x1 quad grid.
indicesFor2x1Quad :: Assertion
indicesFor2x1Quad = do
    let Right imageMap = fromVector (Raw16Vector (3, 2) $ Vector.fromList [0, 0, 0, 0, 0, 0])
        Right terrainGrid = fromImageMap 1 imageMap

    SVector.fromList [ 1, 0, 3, 1, 3, 4
                     , 2, 1, 4, 2, 4, 5 ] @=? indexVector terrainGrid

-- | Generate indices for a 2x2 quad grid.
indicesFor2x2Quad :: Assertion
indicesFor2x2Quad = do
    let Right imageMap =
                fromVector (Raw16Vector (3, 3) $
                    Vector.fromList [ 0, 0, 0, 0, 0, 0 , 0, 0, 0 ])
        Right terrainGrid = fromImageMap 1 imageMap

    SVector.fromList [ 1, 0, 3, 1, 3, 4
                     , 2, 1, 4, 2, 4, 5
                     , 4, 3, 6, 4, 6, 7
                     , 5, 4, 7, 5, 7, 8 ] @=? indexVector terrainGrid

-- | Export a terrain map as a Vert_P.
exportAsVertP :: Assertion
exportAsVertP = do
    let (verts, indices) = asVertP mkTerrainGrid
    9 @=? SVector.length verts
    24 @=? SVector.length indices

    -- Row 1
    let v0 = SVector.unsafeIndex verts 0
    V3 0 0 0 @=? Vert_P.position v0

    let v1 = SVector.unsafeIndex verts 1
    V3 1 0 0 @=? Vert_P.position v1

    let v2 = SVector.unsafeIndex verts 2
    V3 2 0 0 @=? Vert_P.position v2

    -- Row 2.
    let v3 = SVector.unsafeIndex verts 3
    V3 0 1 1 @=? Vert_P.position v3

    let v4 = SVector.unsafeIndex verts 4
    V3 1 1 1 @=? Vert_P.position v4

    let v5 = SVector.unsafeIndex verts 5
    V3 2 1 1 @=? Vert_P.position v5

    -- Row 3.
    let v6 = SVector.unsafeIndex verts 6
    V3 0 0 2 @=? Vert_P.position v6

    let v7 = SVector.unsafeIndex verts 7
    V3 1 0 2 @=? Vert_P.position v7

    let v8 = SVector.unsafeIndex verts 8
    V3 2 0 2 @=? Vert_P.position v8

-- | The vertice dimensions of the terrain grid and the image map must equal.
exportAsVertPNTxCFailDimensions :: Assertion
exportAsVertPNTxCFailDimensions = do
    let Right imageMap = fromVector (RGBVector (1, 1) $ Vector.fromList [PixelRGB8 1 1 1] )
    Left "Dimensions must match" @=? asVertPNTxC imageMap mkTerrainGrid

-- | Export a terrain map as a Vert_P.
exportAsVertPNTxC :: Assertion
exportAsVertPNTxC = do
    let Right (verts, indices) = asVertPNTxC mkColorMap mkTerrainGrid
    9 @=? SVector.length verts
    24 @=? SVector.length indices

    -- The expected normals for the smoothed normals.
    let forward = normalize $ V3 0 1 (-1)
        backward = normalize $ V3 0 1 1

    -- Row 1. Normals pointing 45 degrees forward.
    let v0 = SVector.unsafeIndex verts 0
    V3 0 0 0 @=? Vert_P_N_Tx_C.position v0
    forward `equalToVec` Vert_P_N_Tx_C.normal v0
    V2 0 2 @=? Vert_P_N_Tx_C.texCoord v0
    V4 1 0 0 1 @=? Vert_P_N_Tx_C.color v0

    let v1 = SVector.unsafeIndex verts 1
    V3 1 0 0 @=? Vert_P_N_Tx_C.position v1
    forward `equalToVec` Vert_P_N_Tx_C.normal v1
    V2 1 2 @=? Vert_P_N_Tx_C.texCoord v1
    V4 1 0 0 1 @=? Vert_P_N_Tx_C.color v1

    let v2 = SVector.unsafeIndex verts 2
    V3 2 0 0 @=? Vert_P_N_Tx_C.position v2
    forward `equalToVec` Vert_P_N_Tx_C.normal v2
    V2 2 2 @=? Vert_P_N_Tx_C.texCoord v2
    V4 0 0 1 1 @=? Vert_P_N_Tx_C.color v2

    -- Row 2. The normals are a little more complicated as they are
    -- averaged from several surfaces. And as the model look like only the
    -- middle point/normal is completely balanced with regards to the same
    -- number of surfaces.
    let v3 = SVector.unsafeIndex verts 3
    V3 0 1 1 @=? Vert_P_N_Tx_C.position v3
    let slightlyForward = normalize $ forward + forward + backward
    slightlyForward `equalToVec` Vert_P_N_Tx_C.normal v3
    V2 0 1 @=? Vert_P_N_Tx_C.texCoord v3
    V4 1 0 0 1 @=? Vert_P_N_Tx_C.color v3

    let v4 = SVector.unsafeIndex verts 4
    V3 1 1 1 @=? Vert_P_N_Tx_C.position v4
    V3 0 1 0 `equalToVec` Vert_P_N_Tx_C.normal v4
    V2 1 1 @=? Vert_P_N_Tx_C.texCoord v4
    V4 0 0 1 1 @=? Vert_P_N_Tx_C.color v4

    let v5 = SVector.unsafeIndex verts 5
    V3 2 1 1 @=? Vert_P_N_Tx_C.position v5
    let slightlyBackward = normalize $ forward + backward + backward
    slightlyBackward `equalToVec` Vert_P_N_Tx_C.normal v5
    V2 2 1 @=? Vert_P_N_Tx_C.texCoord v5
    V4 0 1 0 1 @=? Vert_P_N_Tx_C.color v5

    -- Row 3. Normals pointing 45 degrees  backward.
    let v6 = SVector.unsafeIndex verts 6
    V3 0 0 2 @=? Vert_P_N_Tx_C.position v6
    backward `equalToVec` Vert_P_N_Tx_C.normal v6
    V2 0 0 @=? Vert_P_N_Tx_C.texCoord v6
    V4 0 0 1 1 @=? Vert_P_N_Tx_C.color v6

    let v7 = SVector.unsafeIndex verts 7
    V3 1 0 2 @=? Vert_P_N_Tx_C.position v7
    backward `equalToVec` Vert_P_N_Tx_C.normal v7
    V2 1 0 @=? Vert_P_N_Tx_C.texCoord v7
    V4 0 1 0 1 @=? Vert_P_N_Tx_C.color v7

    let v8 = SVector.unsafeIndex verts 8
    V3 2 0 2 @=? Vert_P_N_Tx_C.position v8
    backward `equalToVec` Vert_P_N_Tx_C.normal v8
    V2 2 0 @=? Vert_P_N_Tx_C.texCoord v8
    V4 0 1 0 1 @=? Vert_P_N_Tx_C.color v8

-- | Make terrain grid for export testing.
mkTerrainGrid :: TerrainGrid
mkTerrainGrid =
    let Right terrainGrid = fromImageMap 1 mkHeightMap
    in terrainGrid

-- | Make an image map representing a color map with a grid of (3, 3). There
-- are three different color zones.
mkColorMap :: ImageMap
mkColorMap =
    let Right colorMap =
                fromVector (RGBVector (3, 3) $
                    Vector.fromList
                        [ PixelRGB8 255 0 0, PixelRGB8 255 0 0, PixelRGB8 0 0 255
                        , PixelRGB8 255 0 0, PixelRGB8 0 0 255, PixelRGB8 0 255 0
                        , PixelRGB8 0 0 255, PixelRGB8 0 255 0, PixelRGB8 0 255 0
                        ])
    in colorMap

-- | Make an image map representing a height map with a grid of (3, 3). On the
-- second row there's a "ridge".
mkHeightMap :: ImageMap
mkHeightMap =
    let Right heightMap =
                fromVector (Raw16Vector (3, 3) $
                    Vector.fromList [ 0, 0, 0
                                    , 1, 1, 1
                                    , 0, 0, 0
                                    ])
    in heightMap

equalToVec :: (Floating a, Ord a, Show a) => V3 a -> V3 a -> Assertion
equalToVec (V3 x1 y1 z1) (V3 x2 y2 z2) = do
    x1 `equalTo` x2
    y1 `equalTo` y2
    z1 `equalTo` z2

equalTo :: (Floating a, Ord a, Show a) => a -> a -> Assertion
equalTo = assertApproxEqual "Shall be equal" closeEnough

closeEnough :: Floating a => a
closeEnough = 0.00001
