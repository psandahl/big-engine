{-# LANGUAGE OverloadedStrings #-}
module ModelTests
    ( vertexFileParts
    , normalFileParts
    , texCoordFileParts
    , vertexOnlyFaceFileParts
    , vertexNormalFaceFileParts
    , completeFaceFileParts
    , completeModel
    , splittedFileParts
    , assembleVertP
    , assembleVertPN
    , assembleVertPNTx
    ) where

import           Test.HUnit

import qualified BigE.Attribute.Vert_P      as Vert_P
import qualified BigE.Attribute.Vert_P_N    as Vert_P_N
import qualified BigE.Attribute.Vert_P_N_Tx as Vert_P_N_Tx
import qualified BigE.Model.Assembler       as Assembler
import           BigE.Model.Parser          (FilePart (..), Point (..), parser,
                                             splitParts)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Linear                     (V2 (..), V3 (..))
import           Text.Megaparsec            (runParser)

-- | Test parsing of vertex 'FilePart's only.
vertexFileParts :: Assertion
vertexFileParts = do
    let parts1 = runParser parser "test" "v 1.1 2.2 3.3"
        parts2 = runParser parser "test" "v 1.1 2.2 3.3\nv 4.4 5.5 6.6"
    Right [ Vertex $ V3 1.1 2.2 3.3 ] @=? parts1
    Right [ Vertex $ V3 1.1 2.2 3.3
          , Vertex $ V3 4.4 5.5 6.6 ] @=? parts2

-- | Test parsing of vertex 'FilePart's only.
normalFileParts :: Assertion
normalFileParts = do
    let parts1 = runParser parser "test" "vn 1.1 2.2 3.3"
        parts2 = runParser parser "test" "vn 1.1 2.2 3.3\nvn 4.4 5.5 6.6"
    Right [ Normal $ V3 1.1 2.2 3.3 ] @=? parts1
    Right [ Normal $ V3 1.1 2.2 3.3
          , Normal $ V3 4.4 5.5 6.6 ] @=? parts2

-- | Test parsing of texture coordinates 'FilePart's only.
texCoordFileParts :: Assertion
texCoordFileParts = do
  let parts1 = runParser parser "test" "vt 1.1 2.2"
      parts2 = runParser parser "test" "vt 1.1 2.2\nvt 4.4 5.5"
  Right [ TexCoord $ V2 1.1 2.2 ] @=? parts1
  Right [ TexCoord $ V2 1.1 2.2
        , TexCoord $ V2 4.4 5.5 ] @=? parts2

-- | Test parsing of face coordinates for vertex only triangle faces.
vertexOnlyFaceFileParts :: Assertion
vertexOnlyFaceFileParts = do
    let parts1 = runParser parser "test" "f 1// 2// 3//"
        parts2 = runParser parser "test" "f 1// 2// 3//\nf 4// 5// 6//"
    Right [ Triangle (Point 1 Nothing Nothing)
                     (Point 2 Nothing Nothing)
                     (Point 3 Nothing Nothing)
          ] @=? parts1
    Right [ Triangle (Point 1 Nothing Nothing)
                     (Point 2 Nothing Nothing)
                     (Point 3 Nothing Nothing)
          , Triangle (Point 4 Nothing Nothing)
                     (Point 5 Nothing Nothing)
                     (Point 6 Nothing Nothing)
          ] @=? parts2

-- | Test parsing of face coordinates for vertex//normal triangle faces.
vertexNormalFaceFileParts :: Assertion
vertexNormalFaceFileParts = do
  let parts1 = runParser parser "test" "f 1//1 2//2 3//3"
      parts2 = runParser parser "test" "f 1//1 2//2 3//3\nf 4//4 5//5 6//6"
  Right [ Triangle (Point 1 Nothing (Just 1))
                   (Point 2 Nothing (Just 2))
                   (Point 3 Nothing (Just 3))
        ] @=? parts1
  Right [ Triangle (Point 1 Nothing (Just 1))
                   (Point 2 Nothing (Just 2))
                   (Point 3 Nothing (Just 3))
        , Triangle (Point 4 Nothing (Just 4))
                   (Point 5 Nothing (Just 5))
                   (Point 6 Nothing (Just 6))
        ] @=? parts2

-- | Test parsing of face coordinates for vertex/tex/normal triangle faces.
completeFaceFileParts :: Assertion
completeFaceFileParts = do
  let parts1 = runParser parser "test" "f 1/2/3 2/3/4 3/4/5"
      parts2 = runParser parser "test" "f 1/2/3 2/3/4 3/4/5\nf 4/5/6 5/6/7 6/7/8"
  Right [ Triangle (Point 1 (Just 2) (Just 3))
                   (Point 2 (Just 3) (Just 4))
                   (Point 3 (Just 4) (Just 5))
        ] @=? parts1
  Right [ Triangle (Point 1 (Just 2) (Just 3))
                   (Point 2 (Just 3) (Just 4))
                   (Point 3 (Just 4) (Just 5))
        , Triangle (Point 4 (Just 5) (Just 6))
                   (Point 5 (Just 6) (Just 7))
                   (Point 6 (Just 7) (Just 8))
        ] @=? parts2

-- | Split the list of file parts into a tuple of components.
splittedFileParts :: Assertion
splittedFileParts = do
    let (verts, normals, texCoords, triangles) = splitParts modelParts
    [ Vertex $ V3 1 1 1, Vertex $ V3 (-1) 0 (-1) ] @=? verts
    [ Normal $ V3 1 0 0, Normal $ V3 0.7 0.7 0 ] @=? normals
    [ TexCoord $ V2 0 1, TexCoord $ V2 1 0 ] @=? texCoords
    [ Triangle (Point 1 (Just 2) (Just 3)) (Point 2 (Just 3) (Just 4)) (Point 3 (Just 4) (Just 5))
     , Triangle (Point 4 (Just 5) (Just 6)) (Point 5 (Just 6) (Just 7)) (Point 6 (Just 7) (Just 8))] @=? triangles

-- | Test parsing of a complete model.
completeModel :: Assertion
completeModel =
    Right modelParts @=? runParser parser "test" model

-- | Test assembly of a simple model to vert_Ps.
assembleVertP :: Assertion
assembleVertP =
    Just ([ Vert_P.Vertex { Vert_P.position = V3 1 1 0 }
          , Vert_P.Vertex { Vert_P.position = V3 (-1) 1 0 }
          , Vert_P.Vertex { Vert_P.position = V3 (-1) (-1) 0 }
          , Vert_P.Vertex { Vert_P.position = V3 1 (-1) 0 }
          ], [0, 1, 2, 0, 2, 3])
        @=? Assembler.assembleVertP squareParts

-- | Test assembly of a simple model to vert_P_Ns.
assembleVertPN :: Assertion
assembleVertPN =
    Just ([ Vert_P_N.Vertex { Vert_P_N.position = V3 1 1 0
                            , Vert_P_N.normal = V3 0 0 1
                            }
          , Vert_P_N.Vertex { Vert_P_N.position = V3 (-1) 1 0
                            , Vert_P_N.normal = V3 0 0 1
                            }
          , Vert_P_N.Vertex { Vert_P_N.position = V3 (-1) (-1) 0
                            , Vert_P_N.normal = V3 0 0 1
                            }
          , Vert_P_N.Vertex { Vert_P_N.position = V3 1 (-1) 0
                            , Vert_P_N.normal = V3 0 0 1
                            }
          ], [0, 1, 2, 0, 2, 3])
        @=? Assembler.assembleVertPN squareParts

-- | Test assembly of a simple model to vert_P_N_Txs.
assembleVertPNTx :: Assertion
assembleVertPNTx =
    Just ([ Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 1 1 0
                               , Vert_P_N_Tx.normal = V3 0 0 1
                               , Vert_P_N_Tx.texCoord = V2 1 1
                               }
          , Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 (-1) 1 0
                               , Vert_P_N_Tx.normal = V3 0 0 1
                               , Vert_P_N_Tx.texCoord = V2 0 1
                               }
          , Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 (-1) (-1) 0
                               , Vert_P_N_Tx.normal = V3 0 0 1
                               , Vert_P_N_Tx.texCoord = V2 0 0
                               }
          , Vert_P_N_Tx.Vertex { Vert_P_N_Tx.position = V3 1 (-1) 0
                               , Vert_P_N_Tx.normal = V3 0 0 1
                               , Vert_P_N_Tx.texCoord = V2 1 0
                               }
          ], [0, 1, 2, 0, 2, 3])
        @=? Assembler.assembleVertPNTx squareParts

-- | Dummy model. Makes no sense geometerically.
model :: ByteString
model = LBS.pack $ unlines
    [ "# Test model"
    , "mtllib test.mtl"
    , "# Vertices"
    , "v 1.0 1.0 1.0"
    , "v -1.0 0.0 -1.0"
    , "# Texture coordinates"
    , "vt 0.0 1.0"
    , "vt 1.0 0.0"
    , "# Normals"
    , "vn 1.0 0.0 0.0"
    , "vn 0.7 0.7 0.0"
    , "usemtl test.png"
    , "s off"
    , "# Faces"
    , "f 1/2/3 2/3/4 3/4/5"
    , "f 4/5/6 5/6/7 6/7/8"
    ]

-- | The file parts that should be the result of the above model.
modelParts :: [FilePart]
modelParts =
    [ Mtllib "test.mtl"
    , Vertex $ V3 1 1 1
    , Vertex $ V3 (-1) 0 (-1)
    , TexCoord $ V2 0 1
    , TexCoord $ V2 1 0
    , Normal $ V3 1 0 0
    , Normal $ V3 0.7 0.7 0
    , Usemtl "test.png"
    , Smooth "off"
    , Triangle (Point 1 (Just 2) (Just 3))
               (Point 2 (Just 3) (Just 4))
               (Point 3 (Just 4) (Just 5))
    , Triangle (Point 4 (Just 5) (Just 6))
               (Point 5 (Just 6) (Just 7))
               (Point 6 (Just 7) (Just 8))
    ]

-- | Simple model which shall result in a square.
squareParts :: [FilePart]
squareParts =
    [ Vertex $ V3 1 1 0
    , Vertex $ V3 (-1) 1 0
    , Vertex $ V3 (-1) (-1) 0
    , Vertex $ V3 1 (-1) 0
    , Normal $ V3 0 0 1
    , TexCoord $ V2 0 0
    , TexCoord $ V2 1 0
    , TexCoord $ V2 0 1
    , TexCoord $ V2 1 1
    , Triangle (Point 1 (Just 4) (Just 1))
               (Point 2 (Just 3) (Just 1))
               (Point 3 (Just 1) (Just 1))
    , Triangle (Point 1 (Just 4) (Just 1))
               (Point 3 (Just 1) (Just 1))
               (Point 4 (Just 2) (Just 1))
    ]
