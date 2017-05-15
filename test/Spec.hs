module Main where

import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

import           FontParserTests
import           ImageMapTests
import           MathTests
import           PickIdTests
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
        , testCase "vert_P_N_Tx sizeOf == 32" vertP_N_Tx_sizeOf
        , testCase "Vert_P_N_Tx alignment == 4" vertP_N_Tx_alignment
        , testCase "vertP_N_Tx_encodeDecode equals" vertP_N_Tx_encodeDecode
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
        [ testCase "Created with wrong dimensions" withWrongDimensions
        , testCase "Created with right dimensions" withRightDimensions
        , testCase "Reporting the expected size" reportingSize
        , testCase "Finding elements" findingElements
        ]
    , testGroup "Math tests - angle conversions"
        [ testCase "Conversion to radians" toRadiansConversion
        , testCase "Conversion to degrees" toDegreesConversion
        ]
    , testGroup "Util tests - either tuple bundling"
        [ testCase "eitherTwo" eitherTwo
        , testCase "eitherThree" eitherThree
        ]
    ]
