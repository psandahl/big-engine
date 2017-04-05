-- |
-- Module: Graphics.Big.Mesh.Vert_P_Tx
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.Mesh.Vert_P_Tx
    ( Vertex (..)
    ) where

import           Control.Monad                  (unless)
import qualified Data.Vector.Storable           as Vector
import           Foreign                        (Storable (..), castPtr,
                                                 plusPtr)
import           Graphics.Big.Mesh              (Attribute (..))
import           Graphics.Big.Mesh.BufferHelper (allocBoundBuffers,
                                                 fillBoundVBO, pointerOffset)
import           Graphics.GL                    (GLfloat)
import qualified Graphics.GL                    as GL
import           Linear                         (V2, V3)

-- | A convenience definition of a vertex containing two attributes.
-- The vertex position and the texture coordinates.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    , texCoord :: !(V2 GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf v = sizeOf (position v) + sizeOf (texCoord v)

    alignment v = alignment $ position v

    peek ptr = do
        p <- peek $ castPtr ptr
        t <- peek $ castPtr (ptr `plusPtr` sizeOf p)
        return Vertex { position = p, texCoord = t }

    poke ptr v = do
        let pPtr = castPtr ptr
            tPtr = castPtr (pPtr `plusPtr` sizeOf (position v))
        poke pPtr $ position v
        poke tPtr $ texCoord v


-- | Attribute instance.
instance Attribute Vertex where
    initAttributes bufferUsage vertices = do
        buffers <- allocBoundBuffers
        unless (Vector.null vertices) $ do
            item <- fillBoundVBO vertices bufferUsage
            let itemSize = fromIntegral $ sizeOf item

            GL.glEnableVertexAttribArray 0
            GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE
                                     itemSize
                                     (pointerOffset 0)

            GL.glEnableVertexAttribArray 1
            GL.glVertexAttribPointer 1 2 GL.GL_FLOAT GL.GL_FALSE
                                     itemSize
                                     (pointerOffset $ sizeOf (position item))

        return buffers
