-- |
-- Module: BigE.Mesh
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Mesh
    ( Mesh
    , Attribute (..)
    , fromVector
    , update
    , enable
    , disable
    , delete
    , render
    ) where

import           BigE.Attribute            (Attribute (..))
import           BigE.Internal.GLResources (deleteVertexArray)
import           BigE.Types                (Buffer (..), BufferUsage,
                                            Primitive (..), ToGLenum (..),
                                            VertexArray (..))
import           Control.Monad             (unless)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Vector.Storable      (Vector)
import qualified Data.Vector.Storable      as Vector
import           Foreign                   (Storable (..))
import           Graphics.GL               (GLuint)
import qualified Graphics.GL               as GL

data Mesh = Mesh
    { vao     :: !VertexArray
    , vbo     :: !Buffer
    , indices :: !(Vector GLuint)
    } deriving Show

-- | Build a 'Mesh' from vectors with vertex attribute data and index data.
fromVector :: (Attribute a, MonadIO m)
                => BufferUsage -> Vector a -> Vector GLuint -> m Mesh
fromVector bufferUsage vertices indices' = do
    (vao', vbo') <- initAttributes bufferUsage vertices
    return Mesh { vao = vao', vbo = vbo', indices = indices' }

-- | Replace the entire content of the mesh. The new mesh is returned.
-- NOTE: The new data must not be larger than the original content.
update :: (Storable a, MonadIO m) => Vector a -> Vector GLuint -> Mesh -> m Mesh
update vertices indices' mesh = do
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
enable :: MonadIO m => Mesh -> m ()
enable mesh = do
    let VertexArray handle = vao mesh
    GL.glBindVertexArray handle

-- | Disable the current 'VertexArray'.
disable :: MonadIO m => m ()
disable = GL.glBindVertexArray 0

-- | Delete the mesh's 'VertexArray'. The mesh cannot be used any longer.
delete :: MonadIO m => Mesh -> m ()
delete = deleteVertexArray . vao

-- | Render the mesh using indexed rendering (glDrawElements)
render :: MonadIO m => Primitive -> Mesh -> m ()
render primitive mesh = liftIO $
    Vector.unsafeWith (indices mesh) $ \ptr -> do
        let len = Vector.length (indices mesh)
        GL.glDrawElements (toGLenum primitive) (fromIntegral len)
                          GL.GL_UNSIGNED_INT ptr
