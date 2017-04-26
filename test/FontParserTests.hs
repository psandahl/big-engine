{-# LANGUAGE OverloadedStrings #-}
module FontParserTests
    ( parseInfo
    , parseSpacing
    , parsePadding
    ) where

import           Test.HUnit
import           Text.Megaparsec          (parseMaybe)

import           BigE.TextRenderer.Font   (Info (..), Padding (..),
                                           Spacing (..))
import qualified BigE.TextRenderer.Parser as Parser

parseInfo :: Assertion
parseInfo = do
    Just (Info "Verdana" 75 False True "" False 100 True 1 (Padding 3 3 3 3) (Spacing 2 2)) @=?
        parseMaybe
            Parser.parseInfo
                "info face=\"Verdana\" size=75 bold=0 italic=1 charset=\"\" unicode=0 stretchH=100 smooth=1 aa=1 padding=3,3,3,3 spacing=2,2"

    Just (Info "Verdana" 75 False True "" False 100 True 1 (Padding 3 3 3 3) (Spacing 2 2)) @=?
        parseMaybe
            Parser.parseInfo
                "info  face = \"Verdana\" size = 75 bold = 0 italic = 1 charset = \"\" unicode = 0 stretchH = 100 smooth = 1 aa = 1 padding = 3,3,3,3 spacing = 2,2"


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
