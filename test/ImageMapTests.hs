module ImageMapTests
    ( withWrongDimensions
    , withRightDimensions
    , reportingSize
    , findingElements
    ) where

import           Test.HUnit

import           BigE.ImageMap (ImageElement (..), VectorSpec (..), elementAt,
                                fromVector, imageSize)
import qualified Data.Vector   as Vector

withWrongDimensions :: Assertion
withWrongDimensions =
    case fromVector (Raw16Vector (1, 1) $ Vector.fromList [1, 2, 3]) of
        Right _ -> assertBool "Shall fail" False
        Left _  -> assertBool "Shall fail" True

withRightDimensions :: Assertion
withRightDimensions =
    case fromVector (Raw16Vector (2, 2) $ Vector.fromList [1, 2, 3, 4]) of
        Right _ -> assertBool "Shall succeed" True
        Left _  -> assertBool "Shall succeed" False

reportingSize :: Assertion
reportingSize = do
    let Right imgMap = fromVector (Raw16Vector (2, 2) $ Vector.fromList [1, 2, 3, 4])
    (2, 2) @=? imageSize imgMap

findingElements :: Assertion
findingElements = do
    let Right imgMap = fromVector (Raw16Vector (3, 2) $ Vector.fromList [1, 2, 3, 4, 5, 6])
    Raw 1 @=? elementAt 0 0 imgMap
    Raw 2 @=? elementAt 1 0 imgMap
    Raw 3 @=? elementAt 2 0 imgMap
    Raw 4 @=? elementAt 0 1 imgMap
    Raw 5 @=? elementAt 1 1 imgMap
    Raw 6 @=? elementAt 2 1 imgMap
