module VertexTests
    ( vertP_sizeOf
    , vertP_alignment
    , vertP_encodeDecode
    ) where

import           Foreign                  (Storable (..), peek, with)
import           Linear                   (V3 (..))
import           Test.HUnit

import qualified Graphics.Big.Mesh.Vert_P as Vert_P

vertP_sizeOf :: Assertion
vertP_sizeOf = 12 @=? sizeOf sampleVertP

vertP_alignment :: Assertion
vertP_alignment = 4 @=? alignment sampleVertP

vertP_encodeDecode :: Assertion
vertP_encodeDecode = do
    let vert = sampleVertP
    vert' <- encodeDecode vert
    vert @=? vert'

sampleVertP :: Vert_P.Vertex
sampleVertP = Vert_P.Vertex { Vert_P.position = V3 1 2 3 }

encodeDecode :: Storable a => a -> IO a
encodeDecode value = with value peek
