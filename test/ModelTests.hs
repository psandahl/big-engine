{-# LANGUAGE OverloadedStrings #-}
module ModelTests
    ( vertexFileParts
    , normalFileParts
    , texCoordFileParts
    ) where

import           Test.HUnit

import           BigE.Model.Parser (FilePart (..), parser)
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
