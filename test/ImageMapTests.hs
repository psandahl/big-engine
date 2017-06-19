module ImageMapTests
    ( withWrongDimensions
    , withRightDimensions
    , reportingSize
    , findingElements
    , elementToRGB
    , elementToRGBA
    ) where

import           Test.HUnit

import           BigE.ImageMap (ImageElement (..), VectorSpec (..), PixelRGB8 (..), elementAt,
                                fromVector, imageSize, toRGB, toRGBA)
import qualified Data.Vector   as Vector
import Linear (V3 (..), V4 (..))

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

elementToRGB :: Assertion
elementToRGB = do
    let col1 = V3 0 0 0 :: V3 Float
        col2 = V3 1 1 1 :: V3 Float
        col3 = V3 1 0 0 :: V3 Float
        col4 = V3 0 1 0 :: V3 Float
        col5 = V3 0 0 1 :: V3 Float

    col1 @=? toRGB (Raw 0)
    col1 @=? toRGB (RGB $ PixelRGB8 0 0 0)

    col2 @=? toRGB (Raw maxBound)
    col2 @=? toRGB (RGB $ PixelRGB8 maxBound maxBound maxBound)

    col3 @=? toRGB (RGB $ PixelRGB8 maxBound 0 0)
    col4 @=? toRGB (RGB $ PixelRGB8 0 maxBound 0)
    col5 @=? toRGB (RGB $ PixelRGB8 0 0 maxBound)


elementToRGBA :: Assertion
elementToRGBA = do
    let col1 = V4 0 0 0 1 :: V4 Float
        col2 = V4 1 1 1 1 :: V4 Float
        col3 = V4 1 0 0 1 :: V4 Float
        col4 = V4 0 1 0 1 :: V4 Float
        col5 = V4 0 0 1 1 :: V4 Float

    col1 @=? toRGBA (Raw 0)
    col1 @=? toRGBA (RGB $ PixelRGB8 0 0 0)

    col2 @=? toRGBA (Raw maxBound)
    col2 @=? toRGBA (RGB $ PixelRGB8 maxBound maxBound maxBound)

    col3 @=? toRGBA (RGB $ PixelRGB8 maxBound 0 0)
    col4 @=? toRGBA (RGB $ PixelRGB8 0 maxBound 0)
    col5 @=? toRGBA (RGB $ PixelRGB8 0 0 maxBound)
