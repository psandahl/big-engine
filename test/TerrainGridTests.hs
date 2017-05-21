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

withTooSmallImageMap :: Assertion
withTooSmallImageMap = do
    let Right imageMap = fromVector (1, 1) $ Vector.fromList [1]
    case fromImageMap 1 imageMap of
        Right _ -> assertBool "Shall fail" False
        Left _  -> assertBool "Shall fail" True

withMinimumImageMap :: Assertion
withMinimumImageMap = do
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [1, 2, 3, 4]
    case fromImageMap 1 imageMap of
        Right _ -> assertBool "Shall succeed" True
        Left _  -> assertBool "Shall succeed" False

reportingSize :: Assertion
reportingSize = do
    let Right imageMap = fromVector (3, 2) $ Vector.fromList [1, 2, 3, 4, 5, 6]
        Right terrainGrid = fromImageMap 1 imageMap
    (3, 2) @=? verticeGridSize terrainGrid
    (2, 1) @=? squareGridSize terrainGrid

checkingContent :: Assertion
checkingContent = do
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [1, 2, 3, 4]
        Right terrainGrid = fromImageMap 1 imageMap
    V3 0 1 0 @=? lookup (0, 0) terrainGrid
    V3 1 2 0 @=? lookup (1, 0) terrainGrid
    V3 0 3 1 @=? lookup (0, 1) terrainGrid
    V3 1 4 1 @=? lookup (1, 1) terrainGrid

indexingOutsideGrid :: Assertion
indexingOutsideGrid = do
    let Right imageMap = fromVector (2, 2) $ Vector.fromList [1, 1, 1, 1]
        Right terrainGrid = fromImageMap 1 imageMap
    0 `equalTo` terrainHeight (0.5, 0.5) terrainGrid

equalTo :: Float -> Float -> Assertion
equalTo = assertApproxEqual "Shall be equal" closeEnough

closeEnough :: Float
closeEnough = 0.00001
