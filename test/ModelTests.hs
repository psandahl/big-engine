{-# LANGUAGE OverloadedStrings #-}
module ModelTests
    ( vertexFileParts
    , normalFileParts
    , texCoordFileParts
    , vertexOnlyFaceFileParts
    , vertexNormalFaceFileParts
    , completeFaceFileParts
    ) where

import           Test.HUnit

import           BigE.Model.Parser (FilePart (..), Point (..), parser)
import           Linear            (V2 (..), V3 (..))
import           Text.Megaparsec   (runParser)

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
