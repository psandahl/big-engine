-- |
-- Module: BigE.Attribute.Vert_P_N_Tx
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Attribute.Vert_P_N_Tx
    ( Vertex (..)
    ) where

import           BigE.Attribute       (Attribute (..), allocBoundBuffers,
                                       fillBoundVBO, pointerOffset)
import           Control.Monad        (unless)
import qualified Data.Vector.Storable as Vector
import           Foreign              (Storable (..), castPtr, plusPtr)
import           Graphics.GL          (GLfloat)
import qualified Graphics.GL          as GL
import           Linear               (V2, V3)

-- | A convenience definition of a vertex containing three attributes.
-- The vertex position, the vertex normal and the texture coordinates.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    , normal   :: !(V3 GLfloat)
    , texCoord :: !(V2 GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf v = sizeOf (position v) + sizeOf (normal v) + sizeOf (texCoord v)

    alignment v = alignment $ position v

    peek ptr = do
        p <- peek $ castPtr ptr
        let nPtr = castPtr (ptr `plusPtr` sizeOf p)
        n <- peek nPtr
        let tPtr = castPtr (nPtr `plusPtr` sizeOf n)
        t <- peek tPtr
        return Vertex { position = p, normal = n, texCoord = t }

    poke ptr v = do
        let pPtr = castPtr ptr
            nPtr = castPtr (pPtr `plusPtr` sizeOf (position v))
            tPtr = castPtr (nPtr `plusPtr` sizeOf (normal v))
        poke pPtr $ position v
        poke nPtr $ normal v
        poke tPtr $ texCoord v

-- | Attribute instance.
instance Attribute Vertex where
    initAttributes bufferUsage vertices = do
        buffers <- allocBoundBuffers
        unless (Vector.null vertices) $ do
            item <- fillBoundVBO vertices bufferUsage
            let itemSize = fromIntegral $ sizeOf item

            -- Position.
            GL.glEnableVertexAttribArray 0
            GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE
                                     itemSize
                                     (pointerOffset 0)

            -- Normal.
            GL.glEnableVertexAttribArray 1
            GL.glVertexAttribPointer 1 3 GL.GL_FLOAT GL.GL_FALSE
                                     itemSize
                                     (pointerOffset $ sizeOf (position item))

            -- Texture coordinates.
            GL.glEnableVertexAttribArray 2
            GL.glVertexAttribPointer 2 2 GL.GL_FLOAT GL.GL_FALSE
                                     itemSize
                                     (pointerOffset $ sizeOf (position item) +
                                                      sizeOf (normal item))

        return buffers
