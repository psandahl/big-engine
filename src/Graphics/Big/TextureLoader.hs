-- |
-- Module: Graphics.Big.TextureLoader
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.TextureLoader
    ( TextureParameters (..)
    , defaultTextureParameters
    , texture2DFromFile
    , readImageRGB8
    , readImageRGB8A
    ) where

import           Codec.Picture
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import qualified Data.Vector.Storable     as Vector
import           Graphics.Big.GLResources (deleteTexture, genTexture)
import           Graphics.Big.Types       (Texture (..), TextureFormat (..),
                                           TextureMagFilter (..),
                                           TextureMinFilter (..),
                                           TextureWrap (..), ToGLint (..))
import           Graphics.GL              (GLfloat)
import qualified Graphics.GL              as GL

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

-- | Default values for the texture parameters. The 'TextureFormat' is set to
-- RGB8, and the other values are set to resonable defaults.
defaultTextureParameters :: TextureParameters
defaultTextureParameters =
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
texture2DFromFile :: MonadIO m => FilePath -> TextureParameters -> m (Either String Texture)
texture2DFromFile file params = do
    tex@(Texture handle) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D handle

    eResult <- load2D file (format params)
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

load2D :: MonadIO m => FilePath -> TextureFormat -> m (Either String ())
load2D file RGB8 = do
    eImage <- readImageRGB8 file
    case eImage of
        Right image -> Right <$> setTexture2DRGB8 image
        Left err    -> return $ Left err

load2D file RGBA8 = do
    eImage <- readImageRGB8A file
    case eImage of
        Right image -> Right <$> setTexture2DRGBA8 image
        Left err    -> return $ Left err

readImageRGB8 :: MonadIO m => FilePath -> m (Either String (Image PixelRGB8))
readImageRGB8 file = fmap convertRGB8 <$> liftIO (readImage file)

readImageRGB8A :: MonadIO m => FilePath -> m (Either String (Image PixelRGBA8))
readImageRGB8A file = fmap convertRGBA8 <$> liftIO (readImage file)

setTexture2DRGB8 :: MonadIO m => Image PixelRGB8 -> m ()
setTexture2DRGB8 image = liftIO $
    Vector.unsafeWith (imageData image) $
        GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_RGB)
                        (fromIntegral $ imageWidth image)
                        (fromIntegral $ imageHeight image) 0
                        GL.GL_RGB GL.GL_UNSIGNED_BYTE

setTexture2DRGBA8 :: MonadIO m => Image PixelRGBA8 -> m ()
setTexture2DRGBA8 image = liftIO $
    Vector.unsafeWith (imageData image) $
        GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_RGBA)
                        (fromIntegral $ imageWidth image)
                        (fromIntegral $ imageHeight image) 0
                        GL.GL_RGBA GL.GL_UNSIGNED_BYTE
