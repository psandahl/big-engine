-- |
-- Module: BigE.Attribute
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Attribute
    ( Attribute (..)
    , allocBoundBuffers
    , fillBoundVBO
    , pointerOffset
    ) where

import           BigE.Internal.GLResources (genBuffer, genVertexArray)
import           BigE.Types                (Buffer (..), BufferUsage,
                                            ToGLenum (..), VertexArray (..))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Vector.Storable      (Vector)
import qualified Data.Vector.Storable      as Vector
import           Foreign                   (Ptr, Storable (..), nullPtr,
                                            plusPtr)
import qualified Graphics.GL               as GL

class Attribute a where
    initAttributes :: MonadIO m => BufferUsage -> Vector a -> m (VertexArray, Buffer)

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
fillBoundVBO :: (Storable a, MonadIO m) => Vector a -> BufferUsage -> m a
fillBoundVBO vertices bufferUsage = liftIO $
    Vector.unsafeWith vertices $ \ptr -> do
        let first = Vector.head vertices
            itemSize = sizeOf first
            storageSize = itemSize * Vector.length vertices
        GL.glBufferData GL.GL_ARRAY_BUFFER (fromIntegral storageSize)
                        ptr (toGLenum bufferUsage)

        return first

-- | Give a byte offset expressed as a pointer.
pointerOffset :: Int -> Ptr a
pointerOffset = plusPtr nullPtr
