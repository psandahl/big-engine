{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module: Graphics.Big.Types
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.Types
    ( ToGLenum (..)
    , ToGLint (..)
    , BufferTarget (..)
    , BufferUsage (..)
    , Primitive (..)
    , ShaderType (..)
    , Location (..)
    , Shader (..)
    , Program (..)
    , Buffer (..)
    , Framebuffer (..)
    , Texture (..)
    , TextureFormat (..)
    , TextureMagFilter (..)
    , TextureMinFilter (..)
    , TextureWrap (..)
    , VertexArray (..)
    , Uniform (..)
    ) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign                (castPtr, with)
import           Graphics.GL            (GLenum, GLfloat, GLint, GLuint)
import qualified Graphics.GL            as GL
import           Linear                 (M44, V2, V3)

class ToGLenum a where
    toGLenum :: a -> GLenum

class ToGLint a where
    toGLint :: a -> GLint

data BufferTarget = ArrayBuffer
    deriving Show

instance ToGLenum BufferTarget where
    toGLenum ArrayBuffer = GL.GL_ARRAY_BUFFER

data BufferUsage
    = DynamicDraw
    | StaticDraw
    deriving Show

instance ToGLenum BufferUsage where
    toGLenum DynamicDraw = GL.GL_DYNAMIC_DRAW
    toGLenum StaticDraw  = GL.GL_STATIC_DRAW

data ShaderType
    = VertexShader
    | FragmentShader
    deriving Show

instance ToGLenum ShaderType where
    toGLenum VertexShader   = GL.GL_VERTEX_SHADER
    toGLenum FragmentShader = GL.GL_FRAGMENT_SHADER

data Primitive
    = Triangles
    deriving Show

instance ToGLenum Primitive where
    toGLenum Triangles = GL.GL_TRIANGLES

newtype Location = Location GLint
    deriving Show

newtype Shader = Shader GLuint
    deriving Show

newtype Program = Program GLuint
    deriving Show

newtype Buffer = Buffer GLuint
    deriving Show

newtype Framebuffer = Framebuffer GLuint
    deriving Show

newtype Texture = Texture GLuint
    deriving Show

data TextureFormat
    = RGB8
    | RGBA8
    deriving Show

data TextureMagFilter
    = MagLinear
    | MagNearest
    deriving Show

instance ToGLint TextureMagFilter where
    toGLint MagLinear  = fromIntegral GL.GL_LINEAR
    toGLint MagNearest = fromIntegral GL.GL_NEAREST

data TextureMinFilter
    = MinLinear
    | MinNearest
    | MinNearestMipmapNearest
    | MinLinearMipmapNearest
    | MinNearestMipmapLinear
    | MinLinearMipmapLinear
    deriving Show

instance ToGLint TextureMinFilter where
    toGLint MinLinear  = fromIntegral GL.GL_LINEAR
    toGLint MinNearest = fromIntegral GL.GL_NEAREST
    toGLint MinNearestMipmapNearest = fromIntegral GL.GL_NEAREST_MIPMAP_NEAREST
    toGLint MinLinearMipmapNearest = fromIntegral GL.GL_LINEAR_MIPMAP_NEAREST
    toGLint MinNearestMipmapLinear = fromIntegral GL.GL_NEAREST_MIPMAP_LINEAR
    toGLint MinLinearMipmapLinear = fromIntegral GL.GL_LINEAR_MIPMAP_LINEAR

data TextureWrap
    = WrapRepeat
    | WrapClampToEdge
    | WrapClampToBorder
    deriving Show

instance ToGLint TextureWrap where
    toGLint WrapRepeat        = fromIntegral GL.GL_REPEAT
    toGLint WrapClampToEdge   = fromIntegral GL.GL_CLAMP_TO_EDGE
    toGLint WrapClampToBorder = fromIntegral GL.GL_CLAMP_TO_BORDER

newtype VertexArray = VertexArray GLuint
    deriving Show

-- | Class for setting a uniform value to a location.
class Uniform a where
    setUniform :: MonadIO m => Location -> a -> m ()

-- | Uniform instance for GLfloat.
instance Uniform GLfloat where
    setUniform (Location loc) = GL.glUniform1f loc

-- | Uniform instance for V2 GLfloat.
instance Uniform (V2 GLfloat) where
    setUniform (Location loc) value = liftIO $
        with value $ GL.glUniform2fv loc 1 . castPtr

-- | Uniform instance for V3 GLfloat.
instance Uniform (V3 GLfloat) where
    setUniform (Location loc) value = liftIO $
        with value $ GL.glUniform3fv loc 1 . castPtr

-- | Uniform instance for M44 GLfloat.
instance Uniform (M44 GLfloat) where
    setUniform (Location loc) value = liftIO $
        with value $ GL.glUniformMatrix4fv loc 1 GL.GL_TRUE . castPtr
