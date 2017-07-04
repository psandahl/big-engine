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
    , vertPNFromFile
    , vertPNTxFromFile
    ) where

import qualified BigE.Attribute.Vert_P      as Vert_P
import qualified BigE.Attribute.Vert_P_N    as Vert_P_N
import qualified BigE.Attribute.Vert_P_N_Tx as Vert_P_N_Tx
import           BigE.Model.Assembler       (assembleVertP, assembleVertPN,
                                             assembleVertPNTx)
import           BigE.Model.Parser          (FilePart, fromFile)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Vector.Storable       (Vector, fromList)
import           Foreign                    (Storable)
import           Graphics.GL                (GLuint)

-- | Read a model from the given file. Only use the position attribute. All
-- valid model files shall be able to read.
vertPFromFile :: MonadIO m => FilePath
              -> m (Either String (Vector Vert_P.Vertex, Vector GLuint))
vertPFromFile = readIt assembleVertP

-- | Read a model from the given file. The model file must have the attributes
-- position and normal to be able to load.
vertPNFromFile :: MonadIO m => FilePath
               -> m (Either String (Vector Vert_P_N.Vertex, Vector GLuint))
vertPNFromFile = readIt assembleVertPN

-- | Read a model from the given file. The model file must have the attributes
-- position, normal and texture coordinate to be able to load.
vertPNTxFromFile :: MonadIO m => FilePath
                 -> m (Either String (Vector Vert_P_N_Tx.Vertex, Vector GLuint))
vertPNTxFromFile = readIt assembleVertPNTx

readIt :: (Storable a, Storable b, MonadIO m)
       => ([FilePart] -> Maybe ([a], [b])) -> FilePath
       -> m (Either String (Vector a, Vector b))
readIt assemble file = do
    eParts <- fromFile file
    case eParts of
        Right parts ->
            case assemble parts of
                Just (xs, ys) -> return $ Right (fromList xs, fromList ys)
                Nothing       -> return $ Left "Cannot assemble model parts"

        Left err -> return $ Left err
