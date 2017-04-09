-- |
-- Module: BigE.Attribute.Vert_P_N_Tx
-- Copyright: (c) 2017 Patrik Sandahl
-- Licence: MIT
-- Maintainer: Patrik Sandahl <patrik.sandahl@gmail.com>
-- Stability: experimental
-- Portability: portable
-- Language: Haskell2010
module BigE.Attribute.Vert_P_N_Tx
    ( Vertex (..)
    ) where

--import           BigE.Attribute       (Attribute (..), allocBoundBuffers,
--                                       fillBoundVBO, pointerOffset)
--import           Control.Monad        (unless)
--import qualified Data.Vector.Storable as Vector
import           Foreign     (Storable (..), castPtr, plusPtr)
import           Graphics.GL (GLfloat)
--import qualified Graphics.GL          as GL
import           Linear      (V2, V3)

-- | A convenience definition of a vertex containing three attributes.
-- The vertex position, the vertex normal and the texture coordinates.
data Vertex = Vertex
    { position :: !(V3 GLfloat)
    , normal   :: !(V3 GLfloat)
    , texCoord :: !(V2 GLfloat)
    } deriving (Eq, Show)

-- | Storable instance.
instance Storable Vertex where
    sizeOf v = sizeOf (position v) + sizeOf (normal v) + sizeOf (texCoord v)

    alignment v = alignment $ position v

    peek ptr = do
        p <- peek $ castPtr ptr
        let nPtr = castPtr (ptr `plusPtr` sizeOf p)
        n <- peek nPtr
        let tPtr = castPtr (nPtr `plusPtr` sizeOf n)
        t <- peek tPtr
        return Vertex { position = p, normal = n, texCoord = t }

    poke ptr v = do
        let pPtr = castPtr ptr
            nPtr = castPtr (pPtr `plusPtr` sizeOf (position v))
            tPtr = castPtr (nPtr `plusPtr` sizeOf (normal v))
        poke pPtr $ position v
        poke nPtr $ normal v
        poke tPtr $ texCoord v
