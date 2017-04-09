-- |
-- Module: BigE.Internal.GLResources
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Internal.GLResources
    ( createShader
    , deleteShader
    , createProgram
    , deleteProgram
    , genBuffer
    , deleteBuffer
    , genFramebuffer
    , deleteFramebuffer
    , genTexture
    , bindTexture2D
    , disableTexture2D
    , deleteTexture
    , genVertexArray
    , deleteVertexArray
    ) where

import           BigE.Types             (Buffer (..), Framebuffer (..),
                                         Program (..), Shader (..), ShaderType,
                                         Texture (..), ToGLenum (..),
                                         VertexArray (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Foreign
import           Graphics.GL            (GLsizei, GLuint)
import qualified Graphics.GL            as GL

createShader :: MonadIO m => ShaderType -> m Shader
createShader t = Shader <$> GL.glCreateShader (toGLenum t)

deleteShader :: MonadIO m => Shader -> m ()
deleteShader (Shader s) = GL.glDeleteShader s

createProgram :: MonadIO m => m Program
createProgram = Program <$> GL.glCreateProgram

deleteProgram :: MonadIO m => Program -> m ()
deleteProgram (Program p) = GL.glDeleteProgram p

genBuffer :: MonadIO m => m Buffer
genBuffer = Buffer <$> genName GL.glGenBuffers

deleteBuffer :: MonadIO m => Buffer -> m ()
deleteBuffer (Buffer buffer) = delName GL.glDeleteBuffers buffer

genFramebuffer :: MonadIO m => m Framebuffer
genFramebuffer = Framebuffer <$> genName GL.glGenFramebuffers

deleteFramebuffer :: MonadIO m => Framebuffer -> m ()
deleteFramebuffer (Framebuffer buffer) = delName GL.glDeleteFramebuffers buffer

genTexture :: MonadIO m => m Texture
genTexture = Texture <$> genName GL.glGenTextures

bindTexture2D :: MonadIO m => Int -> Texture -> m ()
bindTexture2D unit (Texture texture) = do
    GL.glActiveTexture $ GL.GL_TEXTURE0 + fromIntegral unit
    GL.glBindTexture GL.GL_TEXTURE_2D texture

disableTexture2D :: MonadIO m => m ()
disableTexture2D = GL.glBindTexture GL.GL_TEXTURE_2D 0

deleteTexture :: MonadIO m => Texture -> m ()
deleteTexture (Texture texture) = delName GL.glDeleteTextures texture

genVertexArray :: MonadIO m => m VertexArray
genVertexArray = VertexArray <$> genName GL.glGenVertexArrays

deleteVertexArray :: MonadIO m => VertexArray -> m ()
deleteVertexArray (VertexArray vao) = delName GL.glDeleteVertexArrays vao

genName :: MonadIO m => (GLsizei -> Ptr GLuint -> IO ()) -> m GLuint
genName ctor = liftIO $
    withArray [0] $ \ptr -> do
        ctor 1 ptr
        head <$> peekArray 1 ptr

delName :: MonadIO m => (GLsizei -> Ptr GLuint -> IO ()) -> GLuint -> m ()
delName dtor item = liftIO $
    withArray [item] $ dtor 1
