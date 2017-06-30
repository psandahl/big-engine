-- |
-- Module: BigE.Model
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable

-- Utilities to read Wavefront model files and produce vectors that can be
-- used to create meshes.
module BigE.Model
    ( vertPFromFile
    , vertPNTxFromFile
    ) where

import qualified BigE.Attribute.Vert_P      as Vert_P
import qualified BigE.Attribute.Vert_P_N_Tx as Vert_P_N_Tx
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Vector.Storable       (Vector)
import           Graphics.GL                (GLuint)

-- | Read a model from the given file. Only use the position attribute. All
-- valid model files shall be able to read.
vertPFromFile :: MonadIO m => FilePath
              -> m (Either String (Vector Vert_P.Vertex, Vector GLuint))
vertPFromFile = undefined

-- | Read a model from the given file. The model file must have the attributes
-- position, normal and texture coordinate to be able to load.
vertPNTxFromFile :: MonadIO m => FilePath
                 -> m (Either String (Vector Vert_P_N_Tx.Vertex, Vector GLuint))
vertPNTxFromFile = undefined
