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
    , enableMesh
    , disableMesh
    , deleteMesh
    , renderMesh
    ) where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Vector.Storable     (Vector)
import qualified Data.Vector.Storable     as Vector
import           Graphics.Big.GLResources (deleteVertexArray)
import           Graphics.Big.Types       (BufferUsage, Primitive (..),
                                           ToGLenum (..), VertexArray (..))
import           Graphics.GL              (GLuint)
import qualified Graphics.GL              as GL

data Mesh = Mesh
    { vao     :: !VertexArray
    , indices :: !(Vector GLuint)
    } deriving Show

class Attribute a where
    attribute :: MonadIO m => BufferUsage -> Vector a -> m VertexArray

-- | Build a 'Mesh' from vectors with vertex attribute data and index data.
meshFromVectors :: (Attribute a, MonadIO m)
            => BufferUsage -> Vector a -> Vector GLuint -> m Mesh
meshFromVectors bufferUsage vertices indices' = do
    vao' <- attribute bufferUsage vertices
    return Mesh { vao = vao', indices = indices' }

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
