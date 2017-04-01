-- |
-- Module: Graphics.Big.Mesh.Vert_P_C
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module Graphics.Big.Mesh.Vert_P_C
    ( Vertex (..)
    ) where

import           Control.Monad            (unless)
import qualified Data.Vector.Storable     as Vector
import           Foreign                  (Storable (..), castPtr, nullPtr,
                                           plusPtr)
import           Graphics.Big.GLResources (genBuffer, genVertexArray)
import           Graphics.Big.Mesh        (Attribute (..))
import           Graphics.Big.Types       (Buffer (..), ToGLenum (..),
                                           VertexArray (..))
import           Graphics.GL              (GLfloat)
import qualified Graphics.GL              as GL
import           Linear                   (V3 (..))

-- | A convenience definition of a vertex containing two attributes.
-- The vertex position and the vertex color.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    , color    :: !(V3 GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf v = sizeOf (position v) + sizeOf (color v)

    alignment v = alignment $ position v

    peek ptr = do
        p <- peek $ castPtr ptr
        c <- peek $ castPtr (ptr `plusPtr` sizeOf p)
        return Vertex { position = p, color = c }

    poke ptr v = do
        let pPtr = castPtr ptr
            cPtr = castPtr (pPtr `plusPtr` sizeOf (position v))
        poke pPtr $ position v
        poke cPtr $ color v
