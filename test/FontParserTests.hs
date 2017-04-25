{-# LANGUAGE OverloadedStrings #-}
module FontParserTests
    ( parseSpacing
    ) where

import           Test.HUnit
import           Text.Megaparsec          (parseMaybe)

import           BigE.TextRenderer.Font   (Spacing (..))
import qualified BigE.TextRenderer.Parser as Parser

parseSpacing :: Assertion
parseSpacing = do
    Just (Spacing 0 0) @=? parseMaybe Parser.parseSpacing "spacing=0,0"
    Just (Spacing 0 0) @=? parseMaybe Parser.parseSpacing "spacing = 0 , 0"
    Just (Spacing (-1) (-2)) @=? parseMaybe Parser.parseSpacing "spacing=-1,-2"
