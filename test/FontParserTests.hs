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
    Just (Info "Verdana") @=?
        parseMaybe Parser.parseInfo "info face=\"Verdana\""

parseSpacing :: Assertion
parseSpacing = do
    Just (Spacing 1 2) @=?
        parseMaybe (Parser.keyValue "spacing" Parser.parseSpacing) "spacing=1,2"

    Just (Spacing 1 2) @=?
        parseMaybe (Parser.keyValue "spacing" Parser.parseSpacing) "spacing = 1 , 2"

    Just (Spacing (-1) (-2)) @=?
        parseMaybe (Parser.keyValue "spacing" Parser.parseSpacing) "spacing=-1,-2"

parsePadding :: Assertion
parsePadding = do
    Just (Padding 1 2 3 4) @=?
        parseMaybe (Parser.keyValue "padding" Parser.parsePadding) "padding=1,2,3,4"

    Just (Padding 1 2 3 4) @=?
        parseMaybe (Parser.keyValue "padding" Parser.parsePadding) "padding = 1 , 2 , 3 , 4"

    -- No negatives.
    Nothing @=?
        parseMaybe (Parser.keyValue "padding" Parser.parsePadding) "padding=-1,2,3,4"
