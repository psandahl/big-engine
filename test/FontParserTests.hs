{-# LANGUAGE OverloadedStrings #-}
module FontParserTests
    ( parseSpacing
    , parsePadding
    ) where

import           Test.HUnit
import           Text.Megaparsec          (parseMaybe)

import           BigE.TextRenderer.Font   (Padding (..), Spacing (..))
import qualified BigE.TextRenderer.Parser as Parser

parseSpacing :: Assertion
parseSpacing = do
    Just (Spacing 1 2) @=? parseMaybe Parser.parseSpacing "spacing=1,2"
    Just (Spacing 1 2) @=? parseMaybe Parser.parseSpacing "spacing = 1 , 2"
    Just (Spacing (-1) (-2)) @=? parseMaybe Parser.parseSpacing "spacing=-1,-2"

parsePadding :: Assertion
parsePadding = do
    Just (Padding 1 2 3 4) @=? parseMaybe Parser.parsePadding "padding=1,2,3,4"
    Just (Padding 1 2 3 4) @=? parseMaybe Parser.parsePadding "padding = 1 , 2 , 3 , 4"
    -- No negatives.
    Nothing @=? parseMaybe Parser.parsePadding "padding=-1,2,3,4"
