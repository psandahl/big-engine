{-# LANGUAGE OverloadedStrings #-}
module FontParserTests
    ( parseFontFile
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
        , characters =
            [ Character
                { charId = 0
                , x = 393
                , y = 142
                , width = 62
                , height = 62
                , xOffset = 6
                , yOffset = 16
                , xAdvance = 78
                , pageNo = 0
                , chnl = 0
                }
            , Character
                { charId = 10
                , x = 0
                , y = 0
                , width = 0
                , height = 0
                , xOffset = -3
                , yOffset = 0
                , xAdvance = 4
                , pageNo = 0
                , chnl = 0
                }
            , Character
                { charId = 32
                , x = 0
                , y = 0
                , width = 0
                , height = 0
                , xOffset = -3
                , yOffset = 0
                , xAdvance = 30
                , pageNo = 0
                , chnl = 0
                }
            ]
        , kerningPairs =
            [ Kerning { first = 88, second = 45, amount = -3 }
            , Kerning { first = 45, second = 97, amount = -1 }
            ]
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
    , "kernings count=2"
    , "kerning first=88 second=45 amount=-3"
    , "kerning first=45 second=97 amount=-1"
    ]
