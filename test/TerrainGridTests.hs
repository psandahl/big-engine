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
    ) where

import           Test.HUnit
import           Test.HUnit.Approx

import           BigE.ImageMap        (fromVector)
import           BigE.TerrainGrid     (fromImageMap, indexVector, lookup,
                                       quadGridSize, terrainHeight,
                                       verticeGridSize)
import qualified Data.Vector          as Vector
import qualified Data.Vector.Storable as SVector
import           Linear               (V3 (..))
import           Prelude              hiding (lookup)

-- | An input ImageMap must be at least 2, 2 big. In this test case
-- the input is too small and the creation shall fail
withTooSmallImageMap :: Assertion
withTooSmallImageMap = do
    let Right imageMap = fromVector (1, 1) $ Vector.fromList [1]
    case fromImageMap 1 imageMap of
        Right _ -> assertBool "Shall fail" False
        Left _  -> assertBool "Shall fail" True

-- | Minimum size of input ImageMap. Creation shall succeed.
withMinimumImageMap :: Assertion
withMinimumImageMap = do
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [1, 2, 3, 4]
    case fromImageMap 1 imageMap of
        Right _ -> assertBool "Shall succeed" True
        Left _  -> assertBool "Shall succeed" False

-- | Check that a TerrainGrid is reporting the expected sizes. One size for
-- the vertice grid and a smaller size for the quad grid.
reportingSize :: Assertion
reportingSize = do
    let Right imageMap = fromVector (3, 2) $ Vector.fromList [1, 2, 3, 4, 5, 6]
        Right terrainGrid = fromImageMap 1 imageMap
    (3, 2) @=? verticeGridSize terrainGrid
    (2, 1) @=? quadGridSize terrainGrid

-- | Verify that the created TerrainGrid has the expected content when looking
-- at the vertices.
checkingContent :: Assertion
checkingContent = do
    let Right imageMap = fromVector (3, 3) $
            Vector.fromList [1, 2, 3, 4, 5, 6, 7, 8, 9]
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
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [1, 1, 1, 1]
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
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [0, 1, 0, 1]
        Right terrainGrid = fromImageMap 1 imageMap
    -- To the left edge. Shall be 0.
    0 `equalTo` terrainHeight (0.0, 0.5) terrainGrid

    -- To the right edge. Shall be close to 0.99.
    0.99 `equalTo` terrainHeight (0.99, 0.5) terrainGrid

-- | Generate indices for a 1x1 quad grid.
indicesFor1x1Quad :: Assertion
indicesFor1x1Quad = do
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [0, 0, 0, 0]
        Right terrainGrid = fromImageMap 1 imageMap

    SVector.fromList [1, 0, 2, 1, 2, 3] @=? indexVector terrainGrid

-- | Generate indices for a 2x1 quad grid.
indicesFor2x1Quad :: Assertion
indicesFor2x1Quad = do
    let Right imageMap = fromVector (3, 2) $ Vector.fromList [0, 0, 0, 0, 0, 0]
        Right terrainGrid = fromImageMap 1 imageMap

    SVector.fromList [ 1, 0, 3, 1, 3, 4
                     , 2, 1, 4, 2, 4, 5 ] @=? indexVector terrainGrid

-- | Generate indices for a 2x2 quad grid.
indicesFor2x2Quad :: Assertion
indicesFor2x2Quad = do
    let Right imageMap = fromVector (3, 3) $ Vector.fromList [ 0, 0, 0, 0, 0, 0
                                                             , 0, 0, 0
                                                             ]
        Right terrainGrid = fromImageMap 1 imageMap

    SVector.fromList [ 1, 0, 3, 1, 3, 4
                     , 2, 1, 4, 2, 4, 5
                     , 4, 3, 6, 4, 6, 7
                     , 5, 4, 7, 5, 7, 8 ] @=? indexVector terrainGrid

equalTo :: Float -> Float -> Assertion
equalTo = assertApproxEqual "Shall be equal" closeEnough

closeEnough :: Float
closeEnough = 0.00001
