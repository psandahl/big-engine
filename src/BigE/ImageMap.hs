-- |
-- Module: BigE.ImageMap
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Image maps are 2D data structures, based on bitmap images, that are used
-- as input for height or colors when creating terrains.
--
-- The 'ImageMap' can be initialized from external files, either RAW16 files
-- or coded RGB files, or initialized from RAW16 vectors.
module BigE.ImageMap
    ( ImageMap
    , FileSpec (..)
    , VectorSpec (..)
    , ImageElement (..)
    , Pixel16
    , PixelRGB8 (..)
    , fromFile
    , fromVector
    , elementAt
    , imageSize
    , toRGB
    , toRGBA
    ) where

import           Codec.Picture              (Image (..), Pixel16, Pixel8,
                                             PixelRGB8 (..), convertRGB8,
                                             pixelAt, readImage)
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Binary.Get            (Get, getWord16le, runGet)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Vector                (Vector, (!))
import qualified Data.Vector                as Vector
import           Linear                     (V3 (..), V4, point)

-- | An image map. Usable for e.g. height maps or color maps.
newtype ImageMap = ImageMap ImageImplementation

-- | Specification of the 'ImageMap' to be read from file. Dimensions are
-- (width, size).
data FileSpec
    = RGB8File !FilePath
    | Raw16File !(Int, Int) !FilePath
    deriving Show

-- | Specification of the 'ImageMap' to be constructed from 'Vector'.
data VectorSpec
    = Raw16Vector !(Int, Int) !(Vector Pixel16)
    | RGBVector !(Int, Int) !(Vector PixelRGB8)
    deriving Show

-- | The value of an image element - "pixel".
data ImageElement
    = Raw !Pixel16
    | RGB !PixelRGB8
    deriving (Eq, Show)

-- | Internal descriptor of a vector constructed image.
data ImageDescriptor a = ImageDescriptor
    { width   :: !Int
    , height  :: !Int
    , storage :: !(Vector a)
    }

-- | The internal representation of an 'ImageMap'.
data ImageImplementation
    = RawImage !(ImageDescriptor Pixel16)
    | RGBVectorImage !(ImageDescriptor PixelRGB8)
    | RGBImage !(Image PixelRGB8)

-- | Create an 'ImageMap' from file.
fromFile :: MonadIO m => FileSpec -> m (Either String ImageMap)

-- Raw file flavour.
fromFile (Raw16File dimensions file) =
    fmap ImageMap <$> fromRawFile dimensions file

-- RGB file. Let JuicyPixels take care of the details.
fromFile (RGB8File file) =
    fmap (ImageMap . RGBImage . convertRGB8) <$> liftIO (readImage file)

-- | Create 'ImageMap' from a Vector. Dimensions are (width, size).
fromVector :: VectorSpec -> Either String ImageMap
fromVector (Raw16Vector dimensions vec) = fromVector' dimensions vec RawImage
fromVector (RGBVector dimensions vec) = fromVector' dimensions vec RGBVectorImage

-- | Help function to make vector initialized image maps.
fromVector' :: (Int, Int) -> Vector a
            -> (ImageDescriptor a -> ImageImplementation) -> Either String ImageMap
fromVector' (w, h) vec ctor
    | w * h == Vector.length vec =
        Right $ ImageMap $
            ctor ImageDescriptor
                { width = w
                , height = h
                , storage = vec
                }
    | otherwise = Left "Specified dimension don't match vector"

-- | Get the element at x, y.
elementAt :: Int -> Int -> ImageMap -> ImageElement

-- Raw file flavour.
elementAt x y (ImageMap (RawImage imageDesc)) =
    let idx = width imageDesc * y + x
        val = storage imageDesc ! idx
    in Raw val

-- RGB vector flavour.
elementAt x y (ImageMap (RGBVectorImage imageDesc)) =
    let idx = width imageDesc * y + x
        val = storage imageDesc ! idx
    in RGB val

-- RGB image. Let JuicyPixels take care of the details.
elementAt x y (ImageMap (RGBImage img)) = RGB $ pixelAt img x y

-- | Get the 'ImageMap's size in pixels, (width, height).
imageSize :: ImageMap -> (Int, Int)
imageSize (ImageMap (RawImage imageDesc)) = (width imageDesc, height imageDesc)
imageSize (ImageMap (RGBVectorImage imageDesc)) = (width imageDesc, height imageDesc)
imageSize (ImageMap (RGBImage img))      = (imageWidth img, imageHeight img)

-- | Convert an 'ImageElement' to a RGB value where each color component is
-- normalized to [0:1].
toRGB :: Floating a => ImageElement -> V3 a
toRGB (Raw value) =
    let value' = fromIntegral value / fromIntegral maxPixel16
    in V3 value' value' value'

toRGB (RGB (PixelRGB8 r g b)) =
    let mv = fromIntegral maxPixel8
        r' = fromIntegral r / mv
        g' = fromIntegral g / mv
        b' = fromIntegral b / mv
    in V3 r' g' b'

-- | Convert an 'ImageElement' to a RGBA value where each color component is
-- normalized to [0:1]. If the underlying value not is on format r,g,b,a the
-- alpha channel will be set to 1.
toRGBA :: Floating a => ImageElement -> V4 a
toRGBA = point . toRGB -- point converts from V3 and set w = 1.

maxPixel8 :: Pixel8
maxPixel8 = maxBound

maxPixel16 :: Pixel16
maxPixel16 = maxBound

-- | Read a raw file.
fromRawFile :: MonadIO m => (Int, Int) -> FilePath
            -> m (Either String ImageImplementation)
fromRawFile (w, h) file = do
    eVector <- liftIO $ tryReadFile file
    case eVector of
        Right vec
            | w * h == Vector.length vec ->
                return $ Right $
                    RawImage ImageDescriptor
                        { width = w
                        , height = h
                        , storage = vec
                        }
            | otherwise -> return $ Left "Specified dimension don't match file"
        Left err -> return $ Left (show err)

tryReadFile :: FilePath -> IO (Either SomeException (Vector Pixel16))
tryReadFile file = try $ toVector <$> ByteString.readFile file

-- | Read a Vector Pixel16 from ByteString. Assume the size of the Vector is
-- half the length of the ByteString, and yes, the ByteString must be
-- fully evaluated to perform this.
toVector :: ByteString -> Vector Pixel16
toVector bs =
    let pxl16Items = fromIntegral $ ByteString.length bs `div` 2
    in runGet (buildVector pxl16Items) bs
    where
        buildVector :: Int -> Get (Vector Pixel16)
        buildVector num = Vector.replicateM num getWord16le
