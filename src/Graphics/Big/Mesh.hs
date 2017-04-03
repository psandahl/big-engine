-- |
-- Module: Graphics.Big.Mesh
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.Mesh
    ( Mesh
    , Attribute (..)
    , meshFromVectors
    , updateMesh
    , enableMesh
    , disableMesh
    , deleteMesh
    , renderMesh
    ) where

import           Control.Monad            (unless)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Vector.Storable     (Vector)
import qualified Data.Vector.Storable     as Vector
import           Foreign                  (Storable (..))
import           Graphics.Big.GLResources (deleteVertexArray)
import           Graphics.Big.Types       (Buffer (..), BufferUsage,
                                           Primitive (..), ToGLenum (..),
                                           VertexArray (..))
import           Graphics.GL              (GLuint)
import qualified Graphics.GL              as GL

data Mesh = Mesh
    { vao     :: !VertexArray
    , vbo     :: !Buffer
    , indices :: !(Vector GLuint)
    } deriving Show

class Attribute a where
    initAttributes :: MonadIO m => BufferUsage -> Vector a -> m (VertexArray, Buffer)

-- | Build a 'Mesh' from vectors with vertex attribute data and index data.
meshFromVectors :: (Attribute a, MonadIO m)
                => BufferUsage -> Vector a -> Vector GLuint -> m Mesh
meshFromVectors bufferUsage vertices indices' = do
    (vao', vbo') <- initAttributes bufferUsage vertices
    return Mesh { vao = vao', vbo = vbo', indices = indices' }

-- | Replace the entire content of the mesh.
updateMesh :: (Storable a, MonadIO m) => Vector a -> Vector GLuint -> Mesh -> m Mesh
updateMesh vertices indices' mesh = do
    unless (Vector.null vertices) $ do
        let Buffer handle = vbo mesh
        GL.glBindBuffer GL.GL_ARRAY_BUFFER handle

        let first = Vector.head vertices
            itemSize = sizeOf first
            totalSize = itemSize * Vector.length vertices
        liftIO $ Vector.unsafeWith vertices $
            GL.glBufferSubData GL.GL_ARRAY_BUFFER 0 (fromIntegral totalSize)

        GL.glBindBuffer GL.GL_ARRAY_BUFFER 0

    return $ mesh { indices = indices' }

-- | Enable the mesh's 'VertexArray'.
enableMesh :: MonadIO m => Mesh -> m ()
enableMesh mesh = do
    let VertexArray handle = vao mesh
    GL.glBindVertexArray handle

-- | Disable the current 'VertexArray'.
disableMesh :: MonadIO m => m ()
disableMesh = GL.glBindVertexArray 0

-- | Delete the mesh's 'VertexArray'. The mesh cannot be used any longer.
deleteMesh :: MonadIO m => Mesh -> m ()
deleteMesh = deleteVertexArray . vao

-- | Render the mesh.
renderMesh :: MonadIO m => Primitive -> Mesh -> m ()
renderMesh primitive mesh = liftIO $
    Vector.unsafeWith (indices mesh) $ \ptr -> do
        let len = Vector.length (indices mesh)
        GL.glDrawElements (toGLenum primitive) (fromIntegral len)
                          GL.GL_UNSIGNED_INT ptr
