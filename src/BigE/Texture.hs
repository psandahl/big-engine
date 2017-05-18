-- |
-- Module: BigE.Texture
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Texture
    ( TextureParameters (..)
    , CubeMapFiles (..)
    , defaultParams2D
    , fromFile2D
    , fromFileCube
    , enable2D
    , disable2D
    , enableCube
    , disableCube
    , delete
    , readImageRGB8
    , readImageRGB8A
    ) where

import           BigE.Internal.GLResources (deleteTexture, genTexture)
import           BigE.Types                (Texture (..), TextureFormat (..),
                                            TextureMagFilter (..),
                                            TextureMinFilter (..),
                                            TextureWrap (..), ToGLint (..))
import           Codec.Picture
import           Control.Monad             (forM, when)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.Vector.Storable      as Vector
import           Graphics.GL               (GLenum, GLfloat)
import qualified Graphics.GL               as GL

-- | User parameters for the loading of a 'Texture'.
data TextureParameters = TextureParameters
    { format     :: !TextureFormat
    , genMipmaps :: !Bool
    , wrapS      :: !TextureWrap
    , wrapT      :: !TextureWrap
    , minFilter  :: !TextureMinFilter
    , magFilter  :: !TextureMagFilter
    , lodBias    :: !GLfloat
    } deriving Show

-- | The six files for the cube map faces.
data CubeMapFiles = CubeMapFiles
    { negativeX :: !FilePath
    , positiveX :: !FilePath
    , negativeY :: !FilePath
    , positiveY :: !FilePath
    , negativeZ :: !FilePath
    , positiveZ :: !FilePath
    } deriving Show

-- | Default values for 2D texture parameters. The 'TextureFormat' is set to
-- RGB8, and the other values are set to resonable defaults.
defaultParams2D :: TextureParameters
defaultParams2D =
    TextureParameters
        { format = RGB8
        , genMipmaps = True
        , wrapS = WrapRepeat
        , wrapT = WrapRepeat
        , minFilter = MinNearestMipmapLinear
        , magFilter = MagLinear
        , lodBias = 0
        }

-- | Load a 2D texture from file, using the given 'TextureParameters'. The textures
-- loaded from this function must be "turned up side down" in the fragment
-- shader by flipping the T value.
fromFile2D :: MonadIO m => FilePath -> TextureParameters -> m (Either String Texture)
fromFile2D file params = do
    tex@(Texture handle) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D handle

    eResult <- load2D GL.GL_TEXTURE_2D file (format params)
    case eResult of
        Right () -> do
            when (genMipmaps params) $
                GL.glGenerateMipmap GL.GL_TEXTURE_2D

            GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (toGLint $ wrapS params)
            GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (toGLint $ wrapT params)
            GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (toGLint $ minFilter params)
            GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (toGLint $ magFilter params)
            GL.glTexParameterf GL.GL_TEXTURE_2D GL.GL_TEXTURE_LOD_BIAS (lodBias params)

            GL.glBindTexture GL.GL_TEXTURE_2D 0
            return $ Right tex

        Left err -> do
            GL.glBindTexture GL.GL_TEXTURE_2D 0
            deleteTexture tex
            return $ Left err

-- | Load a cube map texture from a set of six files.
fromFileCube :: MonadIO m => CubeMapFiles -> TextureFormat -> m (Either String Texture)
fromFileCube files format' = do
    tex@(Texture handle) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_CUBE_MAP handle

    let xs = [ (negativeX files, GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_X)
             , (positiveX files, GL.GL_TEXTURE_CUBE_MAP_POSITIVE_X)
             , (negativeY files, GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Y)
             , (positiveY files, GL.GL_TEXTURE_CUBE_MAP_POSITIVE_Y)
             , (negativeZ files, GL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Z)
             , (positiveZ files, GL.GL_TEXTURE_CUBE_MAP_POSITIVE_Z)
             ]
    eResult <- sequence <$>
        forM xs (\(path, target) -> load2D target path format')
    case eResult of
        Right _  -> do

            GL.glTexParameteri GL.GL_TEXTURE_CUBE_MAP GL.GL_TEXTURE_WRAP_S (toGLint WrapClampToEdge)
            GL.glTexParameteri GL.GL_TEXTURE_CUBE_MAP GL.GL_TEXTURE_WRAP_T (toGLint WrapClampToEdge)
            GL.glTexParameteri GL.GL_TEXTURE_CUBE_MAP GL.GL_TEXTURE_WRAP_R (toGLint WrapClampToEdge)
            GL.glTexParameteri GL.GL_TEXTURE_CUBE_MAP GL.GL_TEXTURE_MIN_FILTER (toGLint MinLinear)
            GL.glTexParameteri GL.GL_TEXTURE_CUBE_MAP GL.GL_TEXTURE_MAG_FILTER (toGLint MagLinear)

            GL.glBindTexture GL.GL_TEXTURE_2D 0
            return $ Right tex

        Left err -> do
            GL.glBindTexture GL.GL_TEXTURE_CUBE_MAP 0
            deleteTexture tex
            return $ Left err

-- | Enable the 2D texture at the given texture unit.
enable2D :: MonadIO m => Int -> Texture -> m ()
enable2D unit (Texture texture) = do
    GL.glActiveTexture $ GL.GL_TEXTURE0 + fromIntegral unit
    GL.glBindTexture GL.GL_TEXTURE_2D texture

-- | Disable the 2D texture at the given texture unit.
disable2D :: MonadIO m => Int -> m ()
disable2D unit = do
    GL.glActiveTexture $ GL.GL_TEXTURE0 + fromIntegral unit
    GL.glBindTexture GL.GL_TEXTURE_2D 0

-- | Enable the cube map texture at the given texture unit.
enableCube :: MonadIO m => Int -> Texture -> m ()
enableCube unit (Texture texture) = do
    GL.glActiveTexture $ GL.GL_TEXTURE0 + fromIntegral unit
    GL.glBindTexture GL.GL_TEXTURE_CUBE_MAP texture

-- | Disable the cube map texture at the given texture unit.
disableCube :: MonadIO m => Int -> m ()
disableCube unit = do
    GL.glActiveTexture $ GL.GL_TEXTURE0 + fromIntegral unit
    GL.glBindTexture GL.GL_TEXTURE_CUBE_MAP 0

-- | Delete the given texture.
delete :: MonadIO m => Texture -> m ()
delete = deleteTexture

readImageRGB8 :: MonadIO m => FilePath -> m (Either String (Image PixelRGB8))
readImageRGB8 file = fmap convertRGB8 <$> liftIO (readImage file)

readImageRGB8A :: MonadIO m => FilePath -> m (Either String (Image PixelRGBA8))
readImageRGB8A file = fmap convertRGBA8 <$> liftIO (readImage file)

load2D :: MonadIO m => GLenum -> FilePath -> TextureFormat -> m (Either String ())
load2D target file RGB8 = do
    eImage <- readImageRGB8 file
    case eImage of
        Right image -> Right <$> setTexture2DRGB8 target image
        Left err    -> return $ Left err

load2D target file RGBA8 = do
    eImage <- readImageRGB8A file
    case eImage of
        Right image -> Right <$> setTexture2DRGBA8 target image
        Left err    -> return $ Left err

setTexture2DRGB8 :: MonadIO m => GLenum -> Image PixelRGB8 -> m ()
setTexture2DRGB8 target image = liftIO $
    Vector.unsafeWith (imageData image) $
        GL.glTexImage2D target 0 (fromIntegral GL.GL_RGB)
                        (fromIntegral $ imageWidth image)
                        (fromIntegral $ imageHeight image) 0
                        GL.GL_RGB GL.GL_UNSIGNED_BYTE

setTexture2DRGBA8 :: MonadIO m => GLenum -> Image PixelRGBA8 -> m ()
setTexture2DRGBA8 target image = liftIO $
    Vector.unsafeWith (imageData image) $
        GL.glTexImage2D target 0 (fromIntegral GL.GL_RGBA)
                        (fromIntegral $ imageWidth image)
                        (fromIntegral $ imageHeight image) 0
                        GL.GL_RGBA GL.GL_UNSIGNED_BYTE
