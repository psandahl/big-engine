-- |
-- Module: Graphics.Big.Mesh.Vert_P
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.Mesh.Vert_P
    ( Vertex (..)
    ) where

import           Control.Monad            (unless)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.Vector.Storable     as Vector
import           Foreign                  (Storable (..), castPtr, nullPtr)
import           Graphics.Big.GLResources (genBuffer, genVertexArray)
import           Graphics.Big.Mesh        (Attribute (..))
import           Graphics.Big.Types       (Buffer (..), ToGLenum (..),
                                           VertexArray (..))
import           Graphics.GL              (GLfloat)
import qualified Graphics.GL              as GL
import           Linear                   (V3 (..))

-- | A convenience definition of a vertex containing one attribute, the position.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    } deriving (Eq, Show)

instance Storable Vertex where
    sizeOf v = sizeOf $ position v
    alignment v = alignment $ position v
    peek ptr = Vertex <$> peek (castPtr ptr)
    poke ptr v = poke (castPtr ptr) $ position v

instance Attribute Vertex where
    attribute bufferUsage vertices = do
        vao@(VertexArray vaoId) <- genVertexArray
        GL.glBindVertexArray vaoId

        Buffer vboId <- genBuffer
        GL.glBindBuffer GL.GL_ARRAY_BUFFER vboId

        unless (Vector.null vertices) $ liftIO $
            Vector.unsafeWith vertices $ \ptr -> do
                let first = Vector.head vertices
                    itemSize = sizeOf first
                    storageSize = itemSize * Vector.length vertices
                GL.glBufferData GL.GL_ARRAY_BUFFER (fromIntegral storageSize)
                                ptr (toGLenum bufferUsage)

                GL.glEnableVertexAttribArray 0
                GL.glVertexAttribPointer 0 3 GL.GL_FLOAT GL.GL_FALSE
                                         (fromIntegral itemSize) nullPtr

        GL.glBindVertexArray 0

        return vao
