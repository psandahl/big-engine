module Main where

import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

import           FontParserTests
import qualified ImageMapTests
import           MathTests
import           ModelTests
import           PickIdTests
import qualified TerrainGridTests
import           UtilTests
import           VertexTests

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "Attribute tests - Storable instance"
        [ testCase "Vert_P sizeOf == 12" vertP_sizeOf
        , testCase "Vert_P alignment == 4" vertP_alignment
        , testCase "vertP_encodeDecode equals" vertP_encodeDecode
        , testCase "Vert_P_C sizeOf == 24" vertP_C_sizeOf
        , testCase "Vert_P_C alignment == 4" vertP_C_alignment
        , testCase "vertP_C_encodeDecode equals" vertP_C_encodeDecode
        , testCase "vert_P_Tx sizeOf == 20" vertP_Tx_sizeOf
        , testCase "Vert_P_Tx alignment == 4" vertP_Tx_alignment
        , testCase "vertP_Tx_encodeDecode equals" vertP_Tx_encodeDecode
        , testCase "vertP_N_sizeOf == 24" vertP_N_sizeOf
        , testCase "vertP_N alignment == 4" vertP_N_alignment
        , testCase "vertP_N_encodeDecode equals" vertP_N_encodeDecode
        , testCase "vert_P_N_Tx sizeOf == 32" vertP_N_Tx_sizeOf
        , testCase "Vert_P_N_Tx alignment == 4" vertP_N_Tx_alignment
        , testCase "vertP_N_Tx_encodeDecode equals" vertP_N_Tx_encodeDecode
        , testCase "vertP_N_Tx_C sizeOf == 48" vertP_N_Tx_C_sizeOf
        , testCase "vertP_N_Tx_C_alignment == 4" vertP_N_Tx_C_alignment
        , testCase "vertP_N_Tx_C_encodeDecode equals" vertP_N_Tx_C_encodeDecode
        ]
    , testGroup "Font Parser tests"
        [ testCase "Parse Font Files" parseFontFile
        , testCase "Parse Font Files - no kerning" parseFontFileNoKerning
        ]
    , testGroup "PickId tests"
        [ testCase "zeroPickId shall be zero" zeroValuedPickId
        , testCase "nextPickId shall increse by one" incresedPickId
        , testCase "literalPickId shall construct correctly" pickIdConstruction
        ]
    , testGroup "ImageMap tests"
        [ testCase "Created with wrong dimensions" ImageMapTests.withWrongDimensions
        , testCase "Created with right dimensions" ImageMapTests.withRightDimensions
        , testCase "Reporting the expected size" ImageMapTests.reportingSize
        , testCase "Finding elements" ImageMapTests.findingElements
        , testCase "ImageElement to RGB" ImageMapTests.elementToRGB
        , testCase "ImageElement to RGBA" ImageMapTests.elementToRGBA
        ]
    , testGroup "TerrainGrid tests - construction"
        [ testCase "Create with too small ImageMap" TerrainGridTests.withTooSmallImageMap
        , testCase "Created with minimum ImageMap" TerrainGridTests.withMinimumImageMap
        , testCase "Reporting the expected sizes" TerrainGridTests.reportingSize
        , testCase "Checking terrain content" TerrainGridTests.checkingContent
        ]
    , testGroup "TerrainGrid tests - height calculation"
        [ testCase "Zero height outside of grid" TerrainGridTests.indexingOutsideGrid
        , testCase "Selecting correct triangle" TerrainGridTests.selectingCorrectTriangle
        ]
    , testGroup "TerrainGrid tests - vector exports"
        [ testCase "Index vector for 1x1 grid" TerrainGridTests.indicesFor1x1Quad
        , testCase "Index vector for 2x1 grid" TerrainGridTests.indicesFor2x1Quad
        , testCase "Index vector for 2x2 grid" TerrainGridTests.indicesFor2x2Quad
        , testCase "As VertP" TerrainGridTests.exportAsVertP
        , testCase "As VertPNTxC fail on dimensions"
                   TerrainGridTests.exportAsVertPNTxCFailDimensions
        , testCase "As VertPNTxC" TerrainGridTests.exportAsVertPNTxC
        ]
    , testGroup "Math tests - angle conversions"
        [ testCase "Conversion to radians" toRadiansConversion
        , testCase "Conversion to degrees" toDegreesConversion
        ]
    , testGroup "Math tests - barycentric coordinates"
        [ testCase "Always height 0 in flat triangle" heightInFlatTriangle
        , testCase "Height in triangle skewed on Z-axis" heightInZSkewedTriangle
        , testCase "Height in triangle skewed in X-axis" heightInXSkewedTriangle
        ]
    , testGroup "Math tests - surface normals"
        [ testCase "Calculate surface normals for front facing" frontFacingNormals
        , testCase "Calculate surface normals for 90 deg away facing triangles"
                   differentFacingNormals
        ]
    , testGroup "Model tests - Wavefront file parsing"
        [ testCase "Parse vertex FileParts" vertexFileParts
        , testCase "Parse normal FileParts" normalFileParts
        , testCase "Parse texCoord FileParts" texCoordFileParts
        , testCase "Parse vertex only faces" vertexOnlyFaceFileParts
        , testCase "Parse vertex//normal faces" vertexNormalFaceFileParts
        , testCase "Parse vertex/tex/normal faces" completeFaceFileParts
        , testCase "Parse a complete model" completeModel
        , testCase "Split a FilePart list" splittedFileParts
        ]
    , testGroup "Model tests - model assembly"
        [ testCase "Assemble vertP" assembleVertP
        , testCase "Assemble vertPN" assembleVertPN
        , testCase "Assemble vertPNTx" assembleVertPNTx
        ]
    , testGroup "Util tests - either tuple bundling"
        [ testCase "eitherTwo" eitherTwo
        , testCase "eitherThree" eitherThree
        , testCase "eitherFour" eitherFour
        ]
    , testGroup "Util tests - clamping"
        [ testCase "clamp" clamp
        ]
    ]
