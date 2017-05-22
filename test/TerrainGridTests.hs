module TerrainGridTests
    ( withTooSmallImageMap
    , withMinimumImageMap
    , reportingSize
    , checkingContent
    , indexingOutsideGrid
    ) where

import           Test.HUnit
import           Test.HUnit.Approx

import           BigE.ImageMap     (fromVector)
import           BigE.TerrainGrid  (fromImageMap, lookup, squareGridSize,
                                    terrainHeight, verticeGridSize)
import qualified Data.Vector       as Vector
import           Linear            (V3 (..))
import           Prelude           hiding (lookup)

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
-- the vertice grid and a smaller size for the square grid.
reportingSize :: Assertion
reportingSize = do
    let Right imageMap = fromVector (3, 2) $ Vector.fromList [1, 2, 3, 4, 5, 6]
        Right terrainGrid = fromImageMap 1 imageMap
    (3, 2) @=? verticeGridSize terrainGrid
    (2, 1) @=? squareGridSize terrainGrid

-- | Verify that the created TerrainGrid has the expected content.
checkingContent :: Assertion
checkingContent = do
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [1, 2, 3, 4]
        Right terrainGrid = fromImageMap 1 imageMap
    V3 0 1 0 @=? lookup (0, 0) terrainGrid
    V3 1 2 0 @=? lookup (1, 0) terrainGrid
    V3 0 3 1 @=? lookup (0, 1) terrainGrid
    V3 1 4 1 @=? lookup (1, 1) terrainGrid

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

equalTo :: Float -> Float -> Assertion
equalTo = assertApproxEqual "Shall be equal" closeEnough

closeEnough :: Float
closeEnough = 0.00001
