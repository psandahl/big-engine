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

import           Foreign     (Storable (..), castPtr)
import           Graphics.GL (GLfloat)
import           Linear      (V3 (..))

-- | A convenience definition of a vertex containing one attribute, the position.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    } deriving (Eq, Show)

instance Storable Vertex where
    sizeOf v = sizeOf $ position v
    alignment v = alignment $ position v
    peek ptr = Vertex <$> peek (castPtr ptr)
    poke ptr v = poke (castPtr ptr) $ position v
