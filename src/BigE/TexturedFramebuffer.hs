-- |
-- Module: BigE.TexturedFramebuffer
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.TexturedFramebuffer
    ( TexturedFramebuffer (..)
    , init
    , enable
    , disable
    , delete
    ) where

import           BigE.Internal.GLResources (deleteFramebuffer, genFramebuffer,
                                            genTexture)
import qualified BigE.Texture              as Texture
import           BigE.Types                (Framebuffer (..), Texture (..),
                                            TextureMagFilter (..),
                                            TextureMinFilter (..),
                                            TextureWrap (..), ToGLint (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Foreign                   (nullPtr)
import qualified Graphics.GL               as GL
import           Prelude                   hiding (init)

-- | A Framebuffer with color and depth attachements as textures.
data TexturedFramebuffer = TexturedFramebuffer
    { frameBuffer :: !Framebuffer
    , color       :: !Texture
    , depth       :: !Texture
    } deriving Show

-- | Initialize the 'Framebuffer' and 'Texture' resources needed.
init :: MonadIO m => Int -> Int -> m (Framebuffer, Texture, Texture)
init width height = do
    -- Create the frame buffer object.
    frameBuffer'@(Framebuffer fbo) <- genFramebuffer
    GL.glBindFramebuffer GL.GL_FRAMEBUFFER fbo

    -- Create the color texture and attach it to the frame buffer.
    colorTexture'@(Texture cTex) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D cTex
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_RGB8)
                    (fromIntegral width) (fromIntegral height)
                    0 GL.GL_RGB GL.GL_UNSIGNED_BYTE nullPtr
    configureTexture
    GL.glFramebufferTexture2D GL.GL_FRAMEBUFFER GL.GL_COLOR_ATTACHMENT0
                              GL.GL_TEXTURE_2D cTex 0

    -- Create the depth texture and attach it to the frame buffer.
    depthTexture'@(Texture dTex) <- genTexture
    GL.glBindTexture GL.GL_TEXTURE_2D dTex
    GL.glTexImage2D GL.GL_TEXTURE_2D 0 (fromIntegral GL.GL_DEPTH_COMPONENT)
                    (fromIntegral width) (fromIntegral height)
                    0 GL.GL_DEPTH_COMPONENT GL.GL_FLOAT nullPtr
    configureTexture
    GL.glFramebufferTexture2D GL.GL_FRAMEBUFFER GL.GL_DEPTH_ATTACHMENT
                              GL.GL_TEXTURE_2D dTex 0

    -- Disable buffer reading.
    GL.glReadBuffer GL.GL_NONE

    -- Set the attached color buffer as draw buffer.
    GL.glDrawBuffer GL.GL_COLOR_ATTACHMENT0

    bufStat <- GL.glCheckFramebufferStatus GL.GL_FRAMEBUFFER
    liftIO $ print (bufStat == GL.GL_FRAMEBUFFER_COMPLETE)
    liftIO $ print bufStat

    -- Restore default frame buffer.
    GL.glBindTexture GL.GL_TEXTURE_2D 0
    GL.glBindFramebuffer GL.GL_FRAMEBUFFER 0

    return (frameBuffer', colorTexture', depthTexture')
    where
      configureTexture = do
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S (toGLint WrapClampToEdge)
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T (toGLint WrapClampToEdge)
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER (toGLint MinNearest)
        GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER (toGLint MagNearest)

-- | Enable the framebuffer. I.e. set it as the current target for rendering.
enable :: MonadIO m => TexturedFramebuffer -> m ()
enable fb = do
    let Framebuffer fbo = frameBuffer fb
    GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER fbo

-- | Disable the framebuffer and re-install the default framebuffer as
-- target for rendering.
disable :: MonadIO m => m ()
disable = GL.glBindFramebuffer GL.GL_DRAW_FRAMEBUFFER 0

-- | Delete the Framebuffer.
delete :: MonadIO m => TexturedFramebuffer -> m ()
delete fb = do
    Texture.delete $ color fb
    Texture.delete $ depth fb
    deleteFramebuffer $ frameBuffer fb
