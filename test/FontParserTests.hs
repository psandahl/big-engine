{-# LANGUAGE OverloadedStrings #-}
module FontParserTests
    ( parseFontFile
    , parseInfo
    , parseSpacing
    , parsePadding
    , parseCommon
    , parsePage
    , parseCharacter
    , parseKerning
    ) where

import           Test.HUnit
import           Text.Megaparsec

import           BigE.TextRenderer.Font     (Character (..), Common (..),
                                             FontFile (..), Info (..),
                                             Kerning (..), Padding (..),
                                             Page (..), Spacing (..))
import qualified BigE.TextRenderer.Parser   as Parser
import           Data.ByteString.Lazy.Char8 (ByteString, pack)

-- | Test parsing of 'FontFile' records.
parseFontFile :: Assertion
parseFontFile =
    Right fontFileRecord @=? parse Parser.parseFontFile "" fontFile

-- | Test parsing of 'Info' records.
parseInfo :: Assertion
parseInfo = do
    Just (Info "Verdana" 75 False True "" False 100 True 1 (Padding 3 3 3 3) (Spacing 2 2)) @=?
        parseMaybe
            Parser.parseInfo
                "info face=\"Verdana\" size=75 bold=0 italic=1 charset=\"\" \
                \unicode=0 stretchH=100 smooth=1 aa=1 padding=3,3,3,3 spacing=2,2"

    -- With some spacing around the tokens.
    Just (Info "Verdana" 75 False True "" False 100 True 1 (Padding 3 3 3 3) (Spacing 2 2)) @=?
        parseMaybe
            Parser.parseInfo
                "info  face = \"Verdana\" size = 75 bold = 0 italic = 1 \
                \charset = \"\" unicode = 0 stretchH = 100 smooth = 1 \
                \aa = 1 padding = 3,3,3,3 spacing = 2,2"

-- | Test parsing of 'Spacing' records.
parseSpacing :: Assertion
parseSpacing = do
    Just (Spacing 1 2) @=?
        parseMaybe (Parser.keyValue "spacing" Parser.parseSpacing) "spacing=1,2"

    -- With some spacing around the tokens.
    Just (Spacing 1 2) @=?
        parseMaybe (Parser.keyValue "spacing" Parser.parseSpacing) "spacing = 1 , 2"

    -- With negative numbers.
    Just (Spacing (-1) (-2)) @=?
        parseMaybe (Parser.keyValue "spacing" Parser.parseSpacing) "spacing=-1,-2"

-- | Test parsing of 'Padding' records.
parsePadding :: Assertion
parsePadding = do
    Just (Padding 1 2 3 4) @=?
        parseMaybe (Parser.keyValue "padding" Parser.parsePadding) "padding=1,2,3,4"

    -- With some spacing around the tokens.
    Just (Padding 1 2 3 4) @=?
        parseMaybe (Parser.keyValue "padding" Parser.parsePadding) "padding = 1 , 2 , 3 , 4"

    -- No negatives are accepted.
    Nothing @=?
        parseMaybe (Parser.keyValue "padding" Parser.parsePadding) "padding=-1,2,3,4"

-- | Test parsing of 'Common' records.
parseCommon :: Assertion
parseCommon = do
    Just (Common 90 75 512 512 1 False) @=?
        parseMaybe Parser.parseCommon
            "common lineHeight=90 base=75 scaleW=512 scaleH=512 pages=1 packed=0"

    -- With some spacing around the tokens.
    Just (Common 90 75 512 512 1 False) @=?
        parseMaybe Parser.parseCommon
            "common lineHeight = 90 base = 75 scaleW = 512 scaleH = 512 pages = 1 packed = 0"

-- | Test parsing of 'Page' records.
parsePage :: Assertion
parsePage = do
    Just (Page 0 "verdana.png") @=?
        parseMaybe Parser.parsePage "page id=0 file=\"verdana.png\""

    -- With some spacing around the tokens.
    Just (Page 0 "verdana.png") @=?
        parseMaybe Parser.parsePage "page id = 0 file = \"verdana.png\""

-- | Test parsing of 'Character' records.
parseCharacter :: Assertion
parseCharacter = do
    Just (Character 65 0 1 512 512 1 56 100 0 0) @=?
        parseMaybe
            Parser.parseCharacter
                "char id=65 x=0 y=1 width=512 height=512 xoffset=1 \
                \yoffset=56 xadvance=100 page=0 chnl=0"

    -- With some spacing around the tokens.
    Just (Character 65 0 1 512 512 1 56 100 0 0) @=?
        parseMaybe
            Parser.parseCharacter
                "char id = 65 x = 0 y = 1 width = 512 height = 512 xoffset = 1 \
                \yoffset = 56 xadvance = 100 page = 0 chnl = 0"

-- | Test parsing of 'Kerning' records.
parseKerning :: Assertion
parseKerning =
    Just (Kerning 1 2 3) @=?
        parseMaybe Parser.parseKerning "kerning first=1 second=2 amount=3"

fontFileRecord :: FontFile
fontFileRecord =
    FontFile
        { info = Info { face = "Verdana"
                      , size = 75
                      , bold = False
                      , italic = False
                      , charset = ""
                      , unicode = False
                      , stretchH = 100
                      , smooth = True
                      , aa = 1
                      , padding = Padding { up = 3
                                          , right = 3
                                          , down = 3
                                          , left = 3
                                          }
                      , spacing = Spacing { horizontal = -2
                                          , vertical = -2
                                          }
                      }
        , common = Common { lineHeight = 95
                          , base = 75
                          , scaleW = 512
                          , scaleH = 512
                          , pages = 1
                          , packed = False
                          }
        , page = Page { pageId = 0
                      , file = "verdana.png"
                      }
        , characters = []
        }

fontFile :: ByteString
fontFile = pack $ unlines
    [ "info face=\"Verdana\" size=75 bold=0 italic=0 charset=\"\" unicode=0 \
          \stretchH=100 smooth=1 aa=1 padding=3,3,3,3 spacing=-2,-2"
    , "common lineHeight=95 base=75 scaleW=512 scaleH=512 pages=1 packed=0"
    , "page id=0 file=\"verdana.png\""
    , "chars count=3"
    , "char id=0       x=393  y=142  width=62   height=62   xoffset=6    yoffset=16   xadvance=78   page=0    chnl=0"
    , "char id=10      x=0    y=0    width=0    height=0    xoffset=-3   yoffset=0    xadvance=4    page=0    chnl=0"
    , "char id=32      x=0    y=0    width=0    height=0    xoffset=-3   yoffset=0    xadvance=30   page=0    chnl=0"
    ]
