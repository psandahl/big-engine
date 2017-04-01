module Main where

import           Test.Framework                 (Test, defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

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
        ]
    ]
