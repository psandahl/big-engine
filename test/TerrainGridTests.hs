module TerrainGridTests
    ( withTooSmallImageMap
    , withMinimumImageMap
    , reportingSize
    ) where

import           Test.HUnit

import           BigE.ImageMap    (fromVector)
import           BigE.TerrainGrid (fromImageMap, squareGridSize,
                                   verticeGridSize)
import qualified Data.Vector      as Vector

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
