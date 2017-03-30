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
    , fromVectors
    ) where

import           Control.Monad.IO.Class   (MonadIO)
import           Data.Vector.Storable     (Vector)
import           Graphics.Big.GLResources (VertexArray)
import           Graphics.GL              (GLuint)

data Mesh = Mesh
    { vao     :: !VertexArray
    , indices :: !(Vector GLuint)
    } deriving Show

fromVectors :: MonadIO m => m Mesh
fromVectors = undefined
