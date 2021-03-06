-- |
-- Module: BigE.Attribute.Vert_P
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
module BigE.Attribute.Vert_P
    ( Vertex (..)
    ) where

import           BigE.Attribute       (Attribute (..), allocBoundBuffers,
                                       fillBoundVBO, pointerOffset)
import           Control.Monad        (unless)
import qualified Data.Vector.Storable as Vector
import           Foreign              (Storable (..), castPtr)
import           Graphics.GL          (GLfloat)
import qualified Graphics.GL          as GL
import           Linear               (V3 (..))

-- | A convenience definition of a vertex containing one attribute, the position.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf v = sizeOf $ position v
    alignment v = alignment $ position v
    peek ptr = Vertex <$> peek (castPtr ptr)
    poke ptr v = poke (castPtr ptr) $ position v

-- | Attribute instance.
instance Attribute Vertex where
    initAttributes bufferUsage vertices = do
        buffers <- allocBoundBuffers

        unless (Vector.null vertices) $ do
            item <- fillBoundVBO vertices bufferUsage
            let itemSize = fromIntegral $ sizeOf item

            GL.glEnableVertexAttribArray 0
            GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE
                                     itemSize (pointerOffset 0)

        GL.glBindVertexArray 0

        return buffers
