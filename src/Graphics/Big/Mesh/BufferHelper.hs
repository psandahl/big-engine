-- |
-- Module: Graphics.Big.Mesh.BufferHelper
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.Mesh.BufferHelper
    ( allocBoundBuffers
    , fillBoundVBO
    , pointerOffset
    ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Vector.Storable     (Vector)
import qualified Data.Vector.Storable     as Vector
import           Foreign                  (Ptr, Storable (..), nullPtr, plusPtr)
import           Graphics.Big.GLResources (genBuffer, genVertexArray)
import           Graphics.Big.Types       (Buffer (..), BufferUsage,
                                           ToGLenum (..), VertexArray (..))
import           Graphics.GL              (GLsizei)
import qualified Graphics.GL              as GL

-- | Allocate one VAO and one VBO. Bind both buffers before returning them.
allocBoundBuffers :: MonadIO m => m (VertexArray, Buffer)
allocBoundBuffers = do
    vao@(VertexArray vaoId) <- genVertexArray
    GL.glBindVertexArray vaoId

    vbo@(Buffer vboId) <- genBuffer
    GL.glBindBuffer GL.GL_ARRAY_BUFFER vboId

    return (vao, vbo)

-- | Fill a 'Buffer' with data. It is required that the buffer is bound as a
-- GL_ARRAY_BUFFER, and that the vertices vector is non empty.
fillBoundVBO :: (Storable a, MonadIO m) => Vector a -> BufferUsage -> m GLsizei
fillBoundVBO vertices bufferUsage = liftIO $ do
    Vector.unsafeWith vertices $ \ptr -> do
        let first = Vector.head vertices
            itemSize = sizeOf first
            storageSize = itemSize * Vector.length vertices
        GL.glBufferData GL.GL_ARRAY_BUFFER (fromIntegral storageSize)
                        ptr (toGLenum bufferUsage)

        return (fromIntegral itemSize)

-- | Give a byte offset expressed as a pointer.
pointerOffset :: Int -> Ptr a
pointerOffset = plusPtr nullPtr
