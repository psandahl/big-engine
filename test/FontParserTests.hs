{-# LANGUAGE OverloadedStrings #-}
module FontParserTests
    ( parseInfo
    , parseSpacing
    , parsePadding
    , parseCommon
    , parsePage
    , parseCharacter
    ) where

import           Test.HUnit
import           Text.Megaparsec          (parseMaybe)

import           BigE.TextRenderer.Font   (Character (..), Common (..),
                                           Info (..), Padding (..), Page (..),
                                           Spacing (..))
import qualified BigE.TextRenderer.Parser as Parser

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
    Just (Character 65 0 1 512 512) @=?
        parseMaybe
            Parser.parseCharacter
                "char id=65 x=0 y=1 width=512 height=512"
