module VertexTests
    ( vertP_sizeOf
    , vertP_alignment
    , vertP_encodeDecode
    , vertP_C_sizeOf
    , vertP_C_alignment
    , vertP_C_encodeDecode
    , vertP_Tx_sizeOf
    , vertP_Tx_alignment
    , vertP_Tx_encodeDecode
    , vertP_N_sizeOf
    , vertP_N_alignment
    , vertP_N_encodeDecode
    , vertP_N_Tx_sizeOf
    , vertP_N_Tx_alignment
    , vertP_N_Tx_encodeDecode
    , vertP_N_Tx_C_sizeOf
    , vertP_N_Tx_C_alignment
    , vertP_N_Tx_C_encodeDecode
    ) where

import           Foreign                      (Storable (..), peek, with)
import           Linear                       (V2 (..), V3 (..), V4 (..))
import           Test.HUnit

import qualified BigE.Attribute.Vert_P        as Vert_P
import qualified BigE.Attribute.Vert_P_C      as Vert_P_C
import qualified BigE.Attribute.Vert_P_N      as Vert_P_N
import qualified BigE.Attribute.Vert_P_N_Tx   as Vert_P_N_Tx
import qualified BigE.Attribute.Vert_P_N_Tx_C as Vert_P_N_Tx_C
import qualified BigE.Attribute.Vert_P_Tx     as Vert_P_Tx

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
vertP_C_sizeOf = 28 @=? sizeOf sampleVertP_C

vertP_C_alignment :: Assertion
vertP_C_alignment = 4 @=? alignment sampleVertP_C

vertP_C_encodeDecode :: Assertion
vertP_C_encodeDecode = do
    let vert = sampleVertP_C
    vert' <- encodeDecode vert
    vert @=? vert'

vertP_Tx_sizeOf :: Assertion
vertP_Tx_sizeOf = 20 @=? sizeOf sampleVertP_Tx

vertP_Tx_alignment :: Assertion
vertP_Tx_alignment = 4 @=? alignment sampleVertP_Tx

vertP_Tx_encodeDecode :: Assertion
vertP_Tx_encodeDecode = do
    let vert = sampleVertP_Tx
    vert' <- encodeDecode vert
    vert @=? vert'

vertP_N_sizeOf :: Assertion
vertP_N_sizeOf = 24 @=? sizeOf sampleVertP_N

vertP_N_alignment :: Assertion
vertP_N_alignment = 4 @=? alignment sampleVertP_N

vertP_N_encodeDecode :: Assertion
vertP_N_encodeDecode = do
    let vert = sampleVertP_N
    vert' <- encodeDecode vert
    vert @=? vert'

vertP_N_Tx_sizeOf :: Assertion
vertP_N_Tx_sizeOf = 32 @=? sizeOf sampleVertP_N_Tx

vertP_N_Tx_alignment :: Assertion
vertP_N_Tx_alignment = 4 @=? alignment sampleVertP_N_Tx

vertP_N_Tx_encodeDecode :: Assertion
vertP_N_Tx_encodeDecode = do
    let vert = sampleVertP_N_Tx
    vert' <- encodeDecode vert
    vert @=? vert'

vertP_N_Tx_C_sizeOf :: Assertion
vertP_N_Tx_C_sizeOf =
    48 @=? sizeOf sampleVertP_N_Tx_C

vertP_N_Tx_C_alignment :: Assertion
vertP_N_Tx_C_alignment = 4 @=? alignment sampleVertP_N_Tx_C

vertP_N_Tx_C_encodeDecode :: Assertion
vertP_N_Tx_C_encodeDecode = do
    let vert = sampleVertP_N_Tx_C
    vert' <- encodeDecode vert
    vert @=? vert'

sampleVertP :: Vert_P.Vertex
sampleVertP = Vert_P.Vertex { Vert_P.position = V3 1 2 3 }

sampleVertP_C :: Vert_P_C.Vertex
sampleVertP_C =
    Vert_P_C.Vertex { Vert_P_C.position = V3 1 2 3
                    , Vert_P_C.color = V4 4 5 6 7
                    }

sampleVertP_Tx :: Vert_P_Tx.Vertex
sampleVertP_Tx =
    Vert_P_Tx.Vertex { Vert_P_Tx.position = V3 1 2 3
                     , Vert_P_Tx.texCoord = V2 4 5
                     }

sampleVertP_N :: Vert_P_N.Vertex
sampleVertP_N =
    Vert_P_N.Vertex { Vert_P_N.position = V3 1 2 3
                    , Vert_P_N.normal = V3 4 5 6
                    }

sampleVertP_N_Tx :: Vert_P_N_Tx.Vertex
sampleVertP_N_Tx =
    Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 1 2 3
                       , Vert_P_N_Tx.normal = V3 4 5 6
                       , Vert_P_N_Tx.texCoord = V2 7 8
                       }

sampleVertP_N_Tx_C :: Vert_P_N_Tx_C.Vertex
sampleVertP_N_Tx_C =
    Vert_P_N_Tx_C.Vertex { Vert_P_N_Tx_C.position = V3 1 2 3
                         , Vert_P_N_Tx_C.normal = V3 4 5 6
                         , Vert_P_N_Tx_C.texCoord = V2 7 8
                         , Vert_P_N_Tx_C.color = V4 9 10 11 12
                         }

encodeDecode :: Storable a => a -> IO a
encodeDecode value = with value peek
