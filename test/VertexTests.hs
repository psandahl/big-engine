module VertexTests
    ( vertP_sizeOf
    , vertP_alignment
    , vertP_encodeDecode
    , vertP_C_sizeOf
    , vertP_C_alignment
    , vertP_C_encodeDecode
    ) where

import           Foreign                    (Storable (..), peek, with)
import           Linear                     (V3 (..))
import           Test.HUnit

import qualified Graphics.Big.Mesh.Vert_P   as Vert_P
import qualified Graphics.Big.Mesh.Vert_P_C as Vert_P_C

vertP_sizeOf :: Assertion
vertP_sizeOf = 12 @=? sizeOf sampleVertP

vertP_alignment :: Assertion
vertP_alignment = 4 @=? alignment sampleVertP

vertP_encodeDecode :: Assertion
vertP_encodeDecode = do
    let vert = sampleVertP
    vert' <- encodeDecode vert
    vert @=? vert'

vertP_C_sizeOf :: Assertion
vertP_C_sizeOf = 24 @=? sizeOf sampleVertP_C

vertP_C_alignment :: Assertion
vertP_C_alignment = 4 @=? alignment sampleVertP_C

vertP_C_encodeDecode :: Assertion
vertP_C_encodeDecode = do
    let vert = sampleVertP_C
    vert' <- encodeDecode vert
    vert @=? vert'

sampleVertP :: Vert_P.Vertex
sampleVertP = Vert_P.Vertex { Vert_P.position = V3 1 2 3 }

sampleVertP_C :: Vert_P_C.Vertex
sampleVertP_C =
    Vert_P_C.Vertex { Vert_P_C.position = V3 1 2 3
                    , Vert_P_C.color = V3 4 5 6
                    }

encodeDecode :: Storable a => a -> IO a
encodeDecode value = with value peek
