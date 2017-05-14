-- |
-- Module: BigE.ImageMap
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.ImageMap
    ( ImageMap
    , MapSpec (..)
    , ImageElement (..)
    , fromFile
    , elementAt
    , imageSize
    ) where

import           Codec.Picture              (Image (..), Pixel16, PixelRGB8,
                                             convertRGB8, pixelAt, readImage)
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Binary.Get            (runGet)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import           Data.Vector                (Vector, (!))
import qualified Data.Vector                as Vector
import           Data.Vector.Binary         (genericGetVector)

-- | An image map. Usable for e.g. height maps or color maps.
newtype ImageMap = ImageMap ImageImplementation

-- | Specification of the 'ImageMap' to be read.
data MapSpec
    = RGB8File !FilePath
    | Raw16File !(Int, Int) !FilePath
    deriving Show

-- | The value of an image element - "pixel".
data ImageElement
    = Raw !Pixel16
    | RGB !PixelRGB8
    deriving Show

-- | The internal representation of an 'ImageMap'.
data ImageImplementation
    = RawImage { width   :: !Int
               , height  :: !Int
               , storage :: !(Vector Pixel16)
               }
    | RGBImage !(Image PixelRGB8)

-- | Read an 'ImageMap' from file.
fromFile :: MonadIO m => MapSpec -> m (Either String ImageMap)

-- Raw file flavour.
fromFile (Raw16File dimensions file) =
    fmap ImageMap <$> fromRawFile dimensions file

-- RGB file. Let JuicyPixels take care of the details.
fromFile (RGB8File file) =
    fmap (ImageMap . RGBImage . convertRGB8) <$> liftIO (readImage file)

-- | Get the element at row, col.
elementAt :: Int -> Int -> ImageMap -> ImageElement

-- Raw file flavour.
elementAt row col (ImageMap (RawImage w _h vec)) =
    let idx = w * row + col
        val = vec ! idx
    in Raw val

-- RGB image. Let JuicyPixels take care of the details.
elementAt row col (ImageMap (RGBImage img)) = RGB $ pixelAt img row col

-- | Get the 'ImageMap's size in pixels, (width, height).
imageSize :: ImageMap -> (Int, Int)
imageSize (ImageMap (RawImage w h _vec)) = (w, h)
imageSize (ImageMap (RGBImage img))      = (imageWidth img, imageHeight img)

-- | Read a raw file.
fromRawFile :: MonadIO m => (Int, Int) -> FilePath
            -> m (Either String ImageImplementation)
fromRawFile (w, h) file = do
    eVector <- liftIO $ tryReadFile file
    case eVector of
        Right vec
            | w * h == Vector.length vec ->
                return $ Right
                    RawImage
                        { width = w
                        , height = h
                        , storage = vec
                        }
            | otherwise -> return $ Left "Specified dimension don't match file"
        Left err -> return $ Left (show err)

tryReadFile :: FilePath -> IO (Either SomeException (Vector Pixel16))
tryReadFile file = try $ toVector <$> ByteString.readFile file

toVector :: ByteString -> Vector Pixel16
toVector = runGet genericGetVector
